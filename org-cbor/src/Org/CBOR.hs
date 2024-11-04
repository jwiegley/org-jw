-- {-# LANGUAGE ApplicativeDo #-}
-- {-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE MultiWayIf #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE TypeOperators #-}

module Org.CBOR (orgFileToCBOR, orgFileFromCBOR) where

import Codec.Serialise
import Data.Bifunctor
import Data.ByteString.Lazy qualified as B
import Org.Types

deriving instance Serialise Config

deriving instance Serialise Time

deriving instance Serialise TimeKind

deriving instance Serialise TimeSuffix

deriving instance Serialise TimeSuffixKind

deriving instance Serialise TimeSpan

deriving instance Serialise Stamp

deriving instance Serialise Duration

deriving instance Serialise Property

deriving instance Serialise Tag

deriving instance Serialise Keyword

deriving instance Serialise Loc

deriving instance Serialise LogEntry

deriving instance Serialise DrawerType

deriving instance Serialise Block

deriving instance Serialise Body

deriving instance Serialise Entry

deriving instance Serialise Header

deriving instance Serialise OrgFile

orgFileToCBOR :: FilePath -> OrgFile -> IO ()
orgFileToCBOR path org = B.writeFile path (serialise org)

orgFileFromCBOR :: FilePath -> IO (Either String OrgFile)
orgFileFromCBOR path = bimap show id . deserialiseOrFail <$> B.readFile path
