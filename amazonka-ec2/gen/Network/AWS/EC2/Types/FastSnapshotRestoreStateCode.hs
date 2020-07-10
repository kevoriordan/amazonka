{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FastSnapshotRestoreStateCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.FastSnapshotRestoreStateCode (
  FastSnapshotRestoreStateCode (
    ..
    , FSRSCDisabled
    , FSRSCDisabling
    , FSRSCEnabled
    , FSRSCEnabling
    , FSRSCOptimizing
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data FastSnapshotRestoreStateCode = FastSnapshotRestoreStateCode' (CI
                                                                     Text)
                                      deriving (Eq, Ord, Read, Show, Data,
                                                Typeable, Generic)

pattern FSRSCDisabled :: FastSnapshotRestoreStateCode
pattern FSRSCDisabled = FastSnapshotRestoreStateCode' "disabled"

pattern FSRSCDisabling :: FastSnapshotRestoreStateCode
pattern FSRSCDisabling = FastSnapshotRestoreStateCode' "disabling"

pattern FSRSCEnabled :: FastSnapshotRestoreStateCode
pattern FSRSCEnabled = FastSnapshotRestoreStateCode' "enabled"

pattern FSRSCEnabling :: FastSnapshotRestoreStateCode
pattern FSRSCEnabling = FastSnapshotRestoreStateCode' "enabling"

pattern FSRSCOptimizing :: FastSnapshotRestoreStateCode
pattern FSRSCOptimizing = FastSnapshotRestoreStateCode' "optimizing"

{-# COMPLETE
  FSRSCDisabled,
  FSRSCDisabling,
  FSRSCEnabled,
  FSRSCEnabling,
  FSRSCOptimizing,
  FastSnapshotRestoreStateCode' #-}

instance FromText FastSnapshotRestoreStateCode where
    parser = (FastSnapshotRestoreStateCode' . mk) <$> takeText

instance ToText FastSnapshotRestoreStateCode where
    toText (FastSnapshotRestoreStateCode' ci) = original ci

-- | Represents an enum of /known/ $FastSnapshotRestoreStateCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FastSnapshotRestoreStateCode where
    toEnum i = case i of
        0 -> FSRSCDisabled
        1 -> FSRSCDisabling
        2 -> FSRSCEnabled
        3 -> FSRSCEnabling
        4 -> FSRSCOptimizing
        _ -> (error . showText) $ "Unknown index for FastSnapshotRestoreStateCode: " <> toText i
    fromEnum x = case x of
        FSRSCDisabled -> 0
        FSRSCDisabling -> 1
        FSRSCEnabled -> 2
        FSRSCEnabling -> 3
        FSRSCOptimizing -> 4
        FastSnapshotRestoreStateCode' name -> (error . showText) $ "Unknown FastSnapshotRestoreStateCode: " <> original name

-- | Represents the bounds of /known/ $FastSnapshotRestoreStateCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FastSnapshotRestoreStateCode where
    minBound = FSRSCDisabled
    maxBound = FSRSCOptimizing

instance Hashable     FastSnapshotRestoreStateCode
instance NFData       FastSnapshotRestoreStateCode
instance ToByteString FastSnapshotRestoreStateCode
instance ToQuery      FastSnapshotRestoreStateCode
instance ToHeader     FastSnapshotRestoreStateCode

instance FromXML FastSnapshotRestoreStateCode where
    parseXML = parseXMLText "FastSnapshotRestoreStateCode"
