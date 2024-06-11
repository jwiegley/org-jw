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
import FlatParse.Combinators
import FlatParse.Stateful hiding (Parser, modify)
import FlatParse.Stateful qualified as FP hiding (modify)
import GHC.Generics
import Org.Data
import Org.Parse
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
parseTagExpr = f TagAnd TagTrue
  where
    f binop end = foldr binop end <$> (sepBy1 go spaces_ <* eof)
    go =
      $( switch
           [|
             case _ of
               "-" -> TagNot . TagVar <$> parseTag
               "(" -> parseTagExpr <* $(char ')')
               "(|" -> f TagOr TagFalse <* $(char ')')
               _ -> TagVar <$> parseTag
             |]
       )

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
    forM_ (cs ^. items) $ \f ->
      when (tagsMatch expr (f ^.. fileTags . traverse)) $ do
        modify succ
        unless dryRun $
          liftIO $
            createLinkInDirectory (f ^. filePath) filterDir

  putStrLn $
    (if dryRun then "Would create " else "Created ")
      ++ show cnt
      ++ " entry links in "
      ++ filterDir
