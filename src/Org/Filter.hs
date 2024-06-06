{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Org.Filter where

import Control.Lens
import Control.Monad (unless, when)
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Data
import Data.Foldable (forM_)
import FlatParse.Stateful hiding (Parser, modify)
import FlatParse.Stateful qualified as FP hiding (modify)
import GHC.Generics
import Org.Data
import Org.Parser
import Org.TagTrees
import Org.Types

data TagExpr
  = TagVar Tag
  | TagAnd TagExpr TagExpr
  | TagOr TagExpr TagExpr
  | TagNot TagExpr
  | TagTrue
  | TagFalse
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Plated)

parseTagExpr :: FP.Parser r String TagExpr
parseTagExpr =
  foldr TagAnd TagTrue <$> (sepBy1 go spaces_ <* eof)
  where
    go = do
      f <- (TagNot <$ $(char '-')) <|> pure id
      f . TagVar <$> parseTag

tagsMatch :: TagExpr -> [Tag] -> Bool
tagsMatch (TagVar tag) ts = tag `elem` ts
tagsMatch (TagAnd e1 e2) ts = tagsMatch e1 ts && tagsMatch e2 ts
tagsMatch (TagOr e1 e2) ts = tagsMatch e1 ts || tagsMatch e2 ts
tagsMatch (TagNot e) ts = not (tagsMatch e ts)
tagsMatch TagTrue _ = True
tagsMatch TagFalse _ = False

makeFilter ::
  Bool -> FilePath -> Bool -> TagExpr -> Collection -> IO ()
makeFilter dryRun filterDir overwrite expr cs = do
  unless dryRun $
    createEmptyDirectory overwrite filterDir

  cnt <- flip execStateT (0 :: Int) $
    forM_ (cs ^. items) $ \f -> do
      let tags = f ^.. fileTags . traverse
      when (tagsMatch expr tags) $ do
        modify succ
        unless dryRun $
          liftIO $
            createLinkInDirectory (f ^. filePath) filterDir

  putStrLn $
    (if dryRun then "Would create " else "Created ")
      ++ show cnt
      ++ " entry links in "
      ++ filterDir
