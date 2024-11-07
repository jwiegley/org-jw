{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stats.Exec where

import Control.Lens hiding ((<.>))
import Control.Monad.State
import Data.Foldable (forM_)
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Org.Data
import Org.Types
import Stats.Options
import System.IO
import Prelude hiding (readFile)

fsize :: FilePath -> IO Integer
fsize path = withFile path ReadMode hFileSize

execStats :: Config -> StatsOptions -> Collection -> IO ()
execStats cfg _opts coll = do
  putStrLn $ show (length orgItems) ++ " files"

  totalSize <- sum <$> mapM (fsize . view orgFilePath) orgItems
  putStrLn $ show (fromIntegral totalSize / 1024.0 / 1024.0 :: Double) ++ " MB"

  putStrLn $ show (length orgEntries) ++ " entries"
  putStrLn $ show (length orgTodos) ++ " items"
  putStrLn $ show (length orgOpenTodos) ++ " open items"
  putStrLn $ show (length orgTodos - length orgOpenTodos) ++ " closed items"

  putStrLn "\nKeywords:"
  showStats allKeywords id

  putStrLn "\nTags:"
  showStats allTags (\(PlainTag t) -> t)

  putStrLn "\nPriorities:"
  showStats (itemsUsed entryPriority) id

  putStrLn "\nVerbs:"
  showStats (itemsUsed entryVerb) id

  putStrLn "\nContexts:"
  showStats (itemsUsed entryContext) id

  putStrLn "\nLocators:"
  showStats (itemsUsed entryLocator) id

  putStrLn "\nDrawers:"
  let drawersUsed = flip execState M.empty $
        forM_ orgEntries $ \ent ->
          forM_
            (ent ^.. entryBody . blocks . traverse . _Drawer . _2 . to show)
            register
  showStats drawersUsed id

  putStrLn "\nProperty keys:"
  let propertiesUsed = flip execState M.empty $ do
        forM_
          ( orgItems
              ^.. traverse
                . orgFileHeader
                . headerPropertiesDrawer
                . traverse
                . name
          )
          register
        forM_
          ( orgItems
              ^.. traverse
                . orgFileHeader
                . headerFileProperties
                . traverse
                . name
          )
          register
        forM_ orgEntries $ \ent ->
          forM_
            (ent ^.. entryProperties . traverse . name)
            register
  showStats propertiesUsed id

  putStrLn "\nLog entry types:"
  let logsUsed = flip execState M.empty $
        forM_ orgEntries $ \ent ->
          forM_
            ( ent
                ^.. entryLogEntries
                  . traverse
                  . cosmos
                  . _LogKey
            )
            register
  showStats logsUsed id

  putStrLn "\nTimestamps:"
  let stampsUsed = flip execState M.empty $
        forM_ orgEntries $ \ent ->
          forM_
            ( ent
                ^.. entryStamps
                  . traverse
                  . cosmos
                  . _StampKey
            )
            register
  showStats stampsUsed id
  where
    _StampKey :: (Applicative f) => (String -> f String) -> Stamp -> f Stamp
    _StampKey f e = case e of
      ClosedStamp _ _ -> e <$ f "Closed"
      ScheduledStamp _ _ -> e <$ f "Scheduled"
      DeadlineStamp _ _ -> e <$ f "Deadline"
      ActiveStamp _ _ -> e <$ f "Active"

    _LogKey :: (Applicative f) => (String -> f String) -> LogEntry -> f LogEntry
    _LogKey f e = case e of
      LogClosing _ _ _ -> e <$ f "LogClosing"
      LogState _ _ _ _ _ -> e <$ f "LogState"
      LogNote _ _ _ -> e <$ f "LogNote"
      LogRescheduled _ _ _ _ -> e <$ f "LogRescheduled"
      LogNotScheduled _ _ _ _ -> e <$ f "LogNotScheduled"
      LogDeadline _ _ _ _ -> e <$ f "LogDeadline"
      LogNoDeadline _ _ _ _ -> e <$ f "LogNoDeadline"
      LogRefiling _ _ _ -> e <$ f "LogRefiling"
      LogClock _ _ _ -> e <$ f "LogClock"
      LogBook _ _ -> e <$ f "LogBook"

    orgItems = coll ^.. items . traverse . _OrgItem
    orgEntries = orgItems ^.. traverse . allEntries
    orgTodos = orgEntries ^.. traverse . keyword . filtered (isTodo cfg)
    orgOpenTodos = orgTodos ^.. traverse . filtered (isOpenTodo cfg)

    allKeywords = countEntries coll $ \e m k ->
      k m $ case e ^. entryKeyword of
        Nothing -> "<plain>"
        Just (OpenKeyword _ kw) -> kw
        Just (ClosedKeyword _ kw) -> kw

    allTags = countEntries coll $ \e m k ->
      foldr (flip k) m (e ^. entryTags)

    register k =
      at k %= \case
        Nothing -> Just (1 :: Int)
        Just n -> Just (succ n)

    itemsUsed l = flip execState M.empty $
      forM_ orgEntries $ \ent ->
        forM_ (ent ^. l) register

    showStats m k =
      forM_ (reverse (sortOn snd (M.assocs m))) $ \(x, n) ->
        putStrLn $ "  " ++ show n ++ " " ++ k x
