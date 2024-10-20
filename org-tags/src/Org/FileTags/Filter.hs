{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Org.FileTags.Filter where

import Control.Monad (unless, when)
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Char (isAlphaNum)
import Data.Data (Data)
import Data.Foldable (forM_)
import Data.Typeable (Typeable)
import FlatParse.Combinators
import FlatParse.Stateful hiding (Parser, modify)
import FlatParse.Stateful qualified as FP hiding (modify)
import GHC.Generics
import Org.FileTags.TagTrees

data TagExpr
  = TagVar String
  | TagAnd TagExpr TagExpr
  | TagOr TagExpr TagExpr
  | TagNot TagExpr
  | TagTrue
  | TagFalse
  deriving (Data, Show, Eq, Typeable, Generic, Ord)

tagName :: FP.Parser r String String
tagName =
  some (satisfy (\c -> isAlphaNum c || c `elem` ['/', ':', '=', '_', ' ']))

parseTagExpr :: FP.Parser r String TagExpr
parseTagExpr = f TagAnd TagTrue
  where
    f binop end = foldr binop end <$> (sepBy1 go spaces_ <* eof)
    go =
      $( switch
           [|
             case _ of
               "-" -> TagNot . TagVar <$> tagName
               "(" -> parseTagExpr <* $(char ')')
               "(|" -> f TagOr TagFalse <* $(char ')')
               _ -> TagVar <$> tagName
             |]
       )

tagsMatch :: TagExpr -> [String] -> Bool
tagsMatch (TagVar tag) ts = tag `elem` ts
tagsMatch (TagAnd e1 e2) ts = tagsMatch e1 ts && tagsMatch e2 ts
tagsMatch (TagOr e1 e2) ts = tagsMatch e1 ts || tagsMatch e2 ts
tagsMatch (TagNot e) ts = not (tagsMatch e ts)
tagsMatch TagTrue _ = True
tagsMatch TagFalse _ = False

makeFilter ::
  Bool -> FilePath -> Bool -> TagExpr -> [FilePath] -> IO ()
makeFilter dryRun filterDir overwrite expr paths = do
  unless dryRun $
    createEmptyDirectory overwrite filterDir

  cnt <- flip execStateT (0 :: Int) $
    forM_ paths $ \path -> do
      tags <- liftIO $ pathTags path
      when (tagsMatch expr tags) $ do
        modify succ
        unless dryRun $
          liftIO $
            createLinkInDirectory path filterDir

  putStrLn $
    (if dryRun then "Would create " else "Created ")
      ++ show cnt
      ++ " entry links in "
      ++ filterDir
