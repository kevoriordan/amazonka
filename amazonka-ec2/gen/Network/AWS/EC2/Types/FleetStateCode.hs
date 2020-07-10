{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetStateCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.FleetStateCode (
  FleetStateCode (
    ..
    , FSCActive
    , FSCDeleted
    , FSCDeletedRunning
    , FSCDeletedTerminating
    , FSCFailed
    , FSCModifying
    , FSCSubmitted
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data FleetStateCode = FleetStateCode' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern FSCActive :: FleetStateCode
pattern FSCActive = FleetStateCode' "active"

pattern FSCDeleted :: FleetStateCode
pattern FSCDeleted = FleetStateCode' "deleted"

pattern FSCDeletedRunning :: FleetStateCode
pattern FSCDeletedRunning = FleetStateCode' "deleted_running"

pattern FSCDeletedTerminating :: FleetStateCode
pattern FSCDeletedTerminating = FleetStateCode' "deleted_terminating"

pattern FSCFailed :: FleetStateCode
pattern FSCFailed = FleetStateCode' "failed"

pattern FSCModifying :: FleetStateCode
pattern FSCModifying = FleetStateCode' "modifying"

pattern FSCSubmitted :: FleetStateCode
pattern FSCSubmitted = FleetStateCode' "submitted"

{-# COMPLETE
  FSCActive,
  FSCDeleted,
  FSCDeletedRunning,
  FSCDeletedTerminating,
  FSCFailed,
  FSCModifying,
  FSCSubmitted,
  FleetStateCode' #-}

instance FromText FleetStateCode where
    parser = (FleetStateCode' . mk) <$> takeText

instance ToText FleetStateCode where
    toText (FleetStateCode' ci) = original ci

-- | Represents an enum of /known/ $FleetStateCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FleetStateCode where
    toEnum i = case i of
        0 -> FSCActive
        1 -> FSCDeleted
        2 -> FSCDeletedRunning
        3 -> FSCDeletedTerminating
        4 -> FSCFailed
        5 -> FSCModifying
        6 -> FSCSubmitted
        _ -> (error . showText) $ "Unknown index for FleetStateCode: " <> toText i
    fromEnum x = case x of
        FSCActive -> 0
        FSCDeleted -> 1
        FSCDeletedRunning -> 2
        FSCDeletedTerminating -> 3
        FSCFailed -> 4
        FSCModifying -> 5
        FSCSubmitted -> 6
        FleetStateCode' name -> (error . showText) $ "Unknown FleetStateCode: " <> original name

-- | Represents the bounds of /known/ $FleetStateCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FleetStateCode where
    minBound = FSCActive
    maxBound = FSCSubmitted

instance Hashable     FleetStateCode
instance NFData       FleetStateCode
instance ToByteString FleetStateCode
instance ToQuery      FleetStateCode
instance ToHeader     FleetStateCode

instance FromXML FleetStateCode where
    parseXML = parseXMLText "FleetStateCode"
