module EmbedRetryTest (tests) where

import Control.Exception (Exception, throwIO, toException, try)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Network.HTTP.Client (
  HttpException (..),
  HttpExceptionContent (..),
  defaultRequest,
 )
import Org.DB.Embed.Retry
import Test.Tasty
import Test.Tasty.HUnit

{- | A stand-in transient exception for exercising the retry loop without a
network.
-}
data Boom = Boom
  deriving (Show)

instance Exception Boom

{- | An action that throws 'Boom' on its first @failures@ invocations and
then returns @val@, recording how many times it has been invoked in
@counter@.
-}
flakyAction :: IORef Int -> Int -> a -> IO a
flakyAction counter failures val = do
  n <- atomicModifyIORef' counter (\x -> (x + 1, x + 1))
  if n <= failures then throwIO Boom else pure val

noSleep :: Int -> IO ()
noSleep _ = pure ()

policy4 :: RetryPolicy
policy4 = RetryPolicy{rpMaxAttempts = 4, rpBaseDelayMicros = 100000}

tests :: TestTree
tests =
  testGroup
    "Embed retry"
    [ testGroup
        "retryingOnWith loop"
        [ testCase "runs the action exactly once on success" $ do
            counter <- newIORef 0
            r <- retryingOnWith noSleep policy4 (const True) (flakyAction counter 0 "ok")
            r @?= ("ok" :: String)
            n <- readIORef counter
            n @?= 1
        , testCase "retries transient failures until the action succeeds" $ do
            counter <- newIORef 0
            r <- retryingOnWith noSleep policy4 (const True) (flakyAction counter 2 "ok")
            r @?= ("ok" :: String)
            n <- readIORef counter
            n @?= 3 -- two failures, then success
        , testCase "gives up after rpMaxAttempts attempts and rethrows" $ do
            counter <- newIORef 0
            let policy = policy4{rpMaxAttempts = 3}
            res <-
              try
                (retryingOnWith noSleep policy (const True) (flakyAction counter 100 "ok")) ::
                IO (Either Boom String)
            case res of
              Left Boom -> pure ()
              Right _ -> assertFailure "expected Boom to be rethrown after exhausting retries"
            n <- readIORef counter
            n @?= 3 -- exactly rpMaxAttempts attempts were made
        , testCase "does not retry a non-transient exception" $ do
            counter <- newIORef 0
            res <-
              try
                (retryingOnWith noSleep policy4 (const False) (flakyAction counter 100 "ok")) ::
                IO (Either Boom String)
            case res of
              Left Boom -> pure ()
              Right _ -> assertFailure "expected Boom to be rethrown immediately"
            n <- readIORef counter
            n @?= 1 -- no retry when the predicate rejects the exception
        , testCase "sleeps with exponential backoff before each retry" $ do
            counter <- newIORef 0
            delays <- newIORef []
            let recordSleep d = modifyIORef' delays (++ [d])
            _ <- retryingOnWith recordSleep policy4 (const True) (flakyAction counter 2 "ok")
            ds <- readIORef delays
            ds @?= [100000, 200000]
        ]
    , testGroup
        "backoffMicros"
        [ testCase "doubles from the base delay" $ do
            backoffMicros policy4 1 @?= 100000
            backoffMicros policy4 2 @?= 200000
            backoffMicros policy4 3 @?= 400000
        ]
    , testGroup
        "isTransientHttpException"
        [ testCase "ConnectionFailure (crypton entropy open failure) is transient" $
            isTransientHttpException
              ( toException
                  ( HttpExceptionRequest
                      defaultRequest
                      ( ConnectionFailure
                          ( toException
                              (userError "crypton: random: cannot get any source of entropy on this system")
                          )
                      )
                  )
              )
              @?= True
        , testCase "ResponseTimeout is transient" $
            isTransientHttpException
              (toException (HttpExceptionRequest defaultRequest ResponseTimeout))
              @?= True
        , testCase "TooManyRedirects is not transient" $
            isTransientHttpException
              (toException (HttpExceptionRequest defaultRequest (TooManyRedirects [])))
              @?= False
        , testCase "InvalidUrlException is not transient" $
            isTransientHttpException (toException (InvalidUrlException "u" "bad"))
              @?= False
        , testCase "a non-HTTP exception is not transient" $
            isTransientHttpException (toException (userError "boom")) @?= False
        ]
    ]
