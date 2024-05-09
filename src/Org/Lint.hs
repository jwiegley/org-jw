{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Org.Lint where

-- import Data.Text.Lazy (Text, pack)
-- import Data.Text.Lazy qualified as T
-- import Data.Time
-- import Org.Types

{-

Linting rules:

- All TODO entries have ID properties, anything recent also has a CREATED
  property.

- No CREATED date lies in the future.

- No property is duplicated.

- No tag is duplicated.

- No title has internal whitespace other than single spaces.

- No title has special characters without escaping.

- Leading and trailing whitespace is consistent within log entries.

- There is no whitespace preceding the event log.

- There is no whitespace after the PROPERTY block (and/or event log) when
  there is no whitespace at the end of the entry.

- If an entry has trailing whitespace, it's siblings have the same whitespace.

- Property blocks are never empty.

- Only TODO items have SCHEDULED/DEADLINE/CLOSED timestamps.

-}
