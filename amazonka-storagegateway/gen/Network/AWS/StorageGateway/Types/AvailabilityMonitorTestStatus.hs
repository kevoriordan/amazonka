{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.AvailabilityMonitorTestStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StorageGateway.Types.AvailabilityMonitorTestStatus (
  AvailabilityMonitorTestStatus (
    ..
    , Complete
    , Failed
    , Pending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AvailabilityMonitorTestStatus = AvailabilityMonitorTestStatus' (CI
                                                                       Text)
                                       deriving (Eq, Ord, Read, Show, Data,
                                                 Typeable, Generic)

pattern Complete :: AvailabilityMonitorTestStatus
pattern Complete = AvailabilityMonitorTestStatus' "COMPLETE"

pattern Failed :: AvailabilityMonitorTestStatus
pattern Failed = AvailabilityMonitorTestStatus' "FAILED"

pattern Pending :: AvailabilityMonitorTestStatus
pattern Pending = AvailabilityMonitorTestStatus' "PENDING"

{-# COMPLETE
  Complete,
  Failed,
  Pending,
  AvailabilityMonitorTestStatus' #-}

instance FromText AvailabilityMonitorTestStatus where
    parser = (AvailabilityMonitorTestStatus' . mk) <$> takeText

instance ToText AvailabilityMonitorTestStatus where
    toText (AvailabilityMonitorTestStatus' ci) = original ci

-- | Represents an enum of /known/ $AvailabilityMonitorTestStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AvailabilityMonitorTestStatus where
    toEnum i = case i of
        0 -> Complete
        1 -> Failed
        2 -> Pending
        _ -> (error . showText) $ "Unknown index for AvailabilityMonitorTestStatus: " <> toText i
    fromEnum x = case x of
        Complete -> 0
        Failed -> 1
        Pending -> 2
        AvailabilityMonitorTestStatus' name -> (error . showText) $ "Unknown AvailabilityMonitorTestStatus: " <> original name

-- | Represents the bounds of /known/ $AvailabilityMonitorTestStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AvailabilityMonitorTestStatus where
    minBound = Complete
    maxBound = Pending

instance Hashable     AvailabilityMonitorTestStatus
instance NFData       AvailabilityMonitorTestStatus
instance ToByteString AvailabilityMonitorTestStatus
instance ToQuery      AvailabilityMonitorTestStatus
instance ToHeader     AvailabilityMonitorTestStatus

instance FromJSON AvailabilityMonitorTestStatus where
    parseJSON = parseJSONText "AvailabilityMonitorTestStatus"
