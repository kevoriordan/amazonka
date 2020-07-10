{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditCheckRunStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AuditCheckRunStatus (
  AuditCheckRunStatus (
    ..
    , ACRSCanceled
    , ACRSCompletedCompliant
    , ACRSCompletedNonCompliant
    , ACRSFailed
    , ACRSInProgress
    , ACRSWaitingForDataCollection
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AuditCheckRunStatus = AuditCheckRunStatus' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern ACRSCanceled :: AuditCheckRunStatus
pattern ACRSCanceled = AuditCheckRunStatus' "CANCELED"

pattern ACRSCompletedCompliant :: AuditCheckRunStatus
pattern ACRSCompletedCompliant = AuditCheckRunStatus' "COMPLETED_COMPLIANT"

pattern ACRSCompletedNonCompliant :: AuditCheckRunStatus
pattern ACRSCompletedNonCompliant = AuditCheckRunStatus' "COMPLETED_NON_COMPLIANT"

pattern ACRSFailed :: AuditCheckRunStatus
pattern ACRSFailed = AuditCheckRunStatus' "FAILED"

pattern ACRSInProgress :: AuditCheckRunStatus
pattern ACRSInProgress = AuditCheckRunStatus' "IN_PROGRESS"

pattern ACRSWaitingForDataCollection :: AuditCheckRunStatus
pattern ACRSWaitingForDataCollection = AuditCheckRunStatus' "WAITING_FOR_DATA_COLLECTION"

{-# COMPLETE
  ACRSCanceled,
  ACRSCompletedCompliant,
  ACRSCompletedNonCompliant,
  ACRSFailed,
  ACRSInProgress,
  ACRSWaitingForDataCollection,
  AuditCheckRunStatus' #-}

instance FromText AuditCheckRunStatus where
    parser = (AuditCheckRunStatus' . mk) <$> takeText

instance ToText AuditCheckRunStatus where
    toText (AuditCheckRunStatus' ci) = original ci

-- | Represents an enum of /known/ $AuditCheckRunStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AuditCheckRunStatus where
    toEnum i = case i of
        0 -> ACRSCanceled
        1 -> ACRSCompletedCompliant
        2 -> ACRSCompletedNonCompliant
        3 -> ACRSFailed
        4 -> ACRSInProgress
        5 -> ACRSWaitingForDataCollection
        _ -> (error . showText) $ "Unknown index for AuditCheckRunStatus: " <> toText i
    fromEnum x = case x of
        ACRSCanceled -> 0
        ACRSCompletedCompliant -> 1
        ACRSCompletedNonCompliant -> 2
        ACRSFailed -> 3
        ACRSInProgress -> 4
        ACRSWaitingForDataCollection -> 5
        AuditCheckRunStatus' name -> (error . showText) $ "Unknown AuditCheckRunStatus: " <> original name

-- | Represents the bounds of /known/ $AuditCheckRunStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AuditCheckRunStatus where
    minBound = ACRSCanceled
    maxBound = ACRSWaitingForDataCollection

instance Hashable     AuditCheckRunStatus
instance NFData       AuditCheckRunStatus
instance ToByteString AuditCheckRunStatus
instance ToQuery      AuditCheckRunStatus
instance ToHeader     AuditCheckRunStatus

instance FromJSON AuditCheckRunStatus where
    parseJSON = parseJSONText "AuditCheckRunStatus"
