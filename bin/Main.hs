{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Lens
import Data.Text (pack)
import GHC.Generics
import Options qualified
import Org.Parser
import Text.Megaparsec
import Text.Show.Pretty

data Config = Config
  {
  }
  deriving (Generic, Show)

newConfig :: Config
newConfig =
  Config
    {
    }

main :: IO ()
main = do
  opts <- Options.getOptions
  case opts ^. Options.command of
    Options.Parse path -> do
      putStrLn $ "Reading Org-mode file " ++ path
      content <- readFile path
      case parse parseOrg path (pack content) of
        Left bundle -> putStr $ errorBundlePretty bundle
        Right org -> pPrint org
