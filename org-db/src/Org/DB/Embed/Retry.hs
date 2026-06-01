{- |
Retry helper for transient HTTP failures during embedding.

The embedding pipeline issues a great many short-lived HTTPS requests. On
macOS the TLS stack (@tls@ → @crypton@) opens @/dev/urandom@ afresh for
every handshake; under concurrency a transient @open()@ failure surfaces as
the misleading @ConnectionFailure user error (crypton: random: cannot get
any source of entropy on this system)@. Because embeddings are idempotent,
the robust response is to retry such transient connection failures rather
than dropping the affected chunks.
-}
module Org.DB.Embed.Retry (
  RetryPolicy (..),
  defaultRetryPolicy,
  backoffMicros,
  retryingOn,
  retryingOnWith,
  isTransientHttpException,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, fromException, throwIO, try)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..))

-- | How an idempotent action should be retried on transient failure.
data RetryPolicy = RetryPolicy
  { rpMaxAttempts :: !Int
  {- ^ Total number of attempts (clamped to at least 1). With @n@ attempts
  there are at most @n - 1@ retries.
  -}
  , rpBaseDelayMicros :: !Int
  {- ^ Base backoff delay, in microseconds. The delay grows exponentially:
  see 'backoffMicros'.
  -}
  }
  deriving (Eq, Show)

{- | Up to 4 attempts (3 retries) with exponential backoff of 100ms, 200ms,
400ms. Sized to ride out the brief device-open spikes that cause the
crypton entropy error without meaningfully slowing a healthy run.
-}
defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy = RetryPolicy{rpMaxAttempts = 4, rpBaseDelayMicros = 100000}

{- | Backoff delay (microseconds) to wait after a failed attempt numbered
@attempt@ (1-based) before making the next one. Doubles each time starting
from 'rpBaseDelayMicros'.
-}
backoffMicros :: RetryPolicy -> Int -> Int
backoffMicros policy attempt =
  rpBaseDelayMicros policy * (2 ^ max 0 (attempt - 1))

{- | Run an idempotent action, retrying on transient failures, using the
supplied sleep function to wait between attempts. Factoring out the sleep
keeps the loop deterministically testable.
-}
retryingOnWith ::
  -- | how to sleep for the given number of microseconds
  (Int -> IO ()) ->
  RetryPolicy ->
  -- | is this exception transient and worth retrying?
  (SomeException -> Bool) ->
  IO a ->
  IO a
retryingOnWith sleep policy isTransient act = go 1
 where
  maxAttempts = max 1 (rpMaxAttempts policy)
  go attempt = do
    result <- try act
    case result of
      Right a -> pure a
      Left err
        | attempt < maxAttempts && isTransient err -> do
            sleep (backoffMicros policy attempt)
            go (attempt + 1)
        | otherwise -> throwIO (err :: SomeException)

{- | Run an idempotent action, retrying transient failures with real
'threadDelay' backoff.
-}
retryingOn :: RetryPolicy -> (SomeException -> Bool) -> IO a -> IO a
retryingOn = retryingOnWith threadDelay

{- | Is this exception a transient HTTP/connection failure worth retrying?

Matches connection-level failures (including the @crypton@ "cannot get any
source of entropy" error, which arrives wrapped as a 'ConnectionFailure'),
timeouts, and prematurely-closed connections. Status-code and
malformed-request errors are deliberately /not/ retried.
-}
isTransientHttpException :: SomeException -> Bool
isTransientHttpException e =
  case fromException e of
    Just (HttpExceptionRequest _ content) -> isTransientContent content
    Just (InvalidUrlException _ _) -> False
    Nothing -> False

isTransientContent :: HttpExceptionContent -> Bool
isTransientContent content =
  case content of
    ConnectionFailure _ -> True
    ResponseTimeout -> True
    ConnectionTimeout -> True
    ConnectionClosed -> True
    NoResponseDataReceived -> True
    IncompleteHeaders -> True
    InternalException _ -> True
    _ -> False
