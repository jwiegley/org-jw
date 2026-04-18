module StampTest (tests) where

import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

loc0 :: Loc
loc0 = Loc "t.org" 0

tm :: Time
tm = Time ActiveTime 60000 Nothing Nothing Nothing Nothing

tests :: TestTree
tests =
  testGroup
    "Stamp helpers"
    [ testGroup
        "isLeadingStamp"
        [ testCase "ClosedStamp is leading" $
            isLeadingStamp (ClosedStamp loc0 tm) @?= True
        , testCase "ScheduledStamp is leading" $
            isLeadingStamp (ScheduledStamp loc0 tm) @?= True
        , testCase "DeadlineStamp is leading" $
            isLeadingStamp (DeadlineStamp loc0 tm) @?= True
        , testCase "ActiveStamp is NOT leading" $
            isLeadingStamp (ActiveStamp loc0 tm) @?= False
        ]
    ]
