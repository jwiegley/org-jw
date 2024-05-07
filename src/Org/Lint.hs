{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Org.Lint where

-- import Data.Text (Text, pack)
-- import Data.Text qualified as T
-- import Data.Time
-- import Org.Types

{-

Linting rules:

- All TODO entries have ID and CREATED properties.

- No CREATED date lies in the future.

- No property is duplicated.

- No tag is duplicated.

- No title has internal whitespace other than single spaces.

- Trailing whitespace is consistent for log entries.

- If an entry's body text has trailing whitespace, it has the same leading
  whitespace.

- Property blocks are never empty.

-}
