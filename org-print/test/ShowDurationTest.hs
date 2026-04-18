module ShowDurationTest (tests) where

import Control.Monad.Reader (runReader)
import Org.Print
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

loc0 :: Loc
loc0 = Loc "t.org" 0

tm :: Time
tm = Time InactiveTime 60000 Nothing Nothing Nothing Nothing

cfg :: Config
cfg = defaultConfig

renderClock :: Duration -> [String]
renderClock d =
  runReader
    (showEntry (entryWith [LogClock loc0 tm (Just d)]))
    cfg

entryWith :: [LogEntry] -> Entry
entryWith les =
  Entry
    { _entryLoc = loc0
    , _entryDepth = 1
    , _entryKeyword = Nothing
    , _entryPriority = Nothing
    , _entryHeadline = "x"
    , _entryVerb = Nothing
    , _entryTitle = "x"
    , _entryContext = Nothing
    , _entryLocator = Nothing
    , _entryTags = []
    , _entryStamps = []
    , _entryProperties = []
    , _entryLogEntries = les
    , _entryBody = Body []
    , _entryItems = []
    }

-- Force full evaluation of each line so HPC marks internal expressions ticked.
forceLines :: [String] -> [String]
forceLines = map (\s -> length s `seq` s)

tests :: TestTree
tests =
  testGroup
    "showDuration"
    [ testCase "single-digit hour pads left with space" $
        let out = forceLines (renderClock (Duration 1 30))
         in assertBool
              (show out)
              ("CLOCK: [2023-02-25 Sat] =>  1:30" `elem` out)
    , testCase "single-digit minutes zero-padded" $
        let out = forceLines (renderClock (Duration 2 5))
         in assertBool
              (show out)
              ("CLOCK: [2023-02-25 Sat] =>  2:05" `elem` out)
    , testCase "two-digit hour keeps both digits" $
        let out = forceLines (renderClock (Duration 10 30))
         in assertBool
              (show out)
              ("CLOCK: [2023-02-25 Sat] => 10:30" `elem` out)
    , testCase "multi-digit hour keeps all digits" $
        let out = forceLines (renderClock (Duration 123 45))
         in assertBool
              (show out)
              ("CLOCK: [2023-02-25 Sat] => 123:45" `elem` out)
    , testCase "zero hours zero minutes pads both" $
        let out = forceLines (renderClock (Duration 0 0))
         in assertBool
              (show out)
              ("CLOCK: [2023-02-25 Sat] =>  0:00" `elem` out)
    ]
