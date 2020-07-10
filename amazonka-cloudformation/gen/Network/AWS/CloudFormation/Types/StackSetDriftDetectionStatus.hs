{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetDriftDetectionStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.StackSetDriftDetectionStatus (
  StackSetDriftDetectionStatus (
    ..
    , SSDDSCompleted
    , SSDDSFailed
    , SSDDSInProgress
    , SSDDSPartialSuccess
    , SSDDSStopped
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StackSetDriftDetectionStatus = StackSetDriftDetectionStatus' (CI
                                                                     Text)
                                      deriving (Eq, Ord, Read, Show, Data,
                                                Typeable, Generic)

pattern SSDDSCompleted :: StackSetDriftDetectionStatus
pattern SSDDSCompleted = StackSetDriftDetectionStatus' "COMPLETED"

pattern SSDDSFailed :: StackSetDriftDetectionStatus
pattern SSDDSFailed = StackSetDriftDetectionStatus' "FAILED"

pattern SSDDSInProgress :: StackSetDriftDetectionStatus
pattern SSDDSInProgress = StackSetDriftDetectionStatus' "IN_PROGRESS"

pattern SSDDSPartialSuccess :: StackSetDriftDetectionStatus
pattern SSDDSPartialSuccess = StackSetDriftDetectionStatus' "PARTIAL_SUCCESS"

pattern SSDDSStopped :: StackSetDriftDetectionStatus
pattern SSDDSStopped = StackSetDriftDetectionStatus' "STOPPED"

{-# COMPLETE
  SSDDSCompleted,
  SSDDSFailed,
  SSDDSInProgress,
  SSDDSPartialSuccess,
  SSDDSStopped,
  StackSetDriftDetectionStatus' #-}

instance FromText StackSetDriftDetectionStatus where
    parser = (StackSetDriftDetectionStatus' . mk) <$> takeText

instance ToText StackSetDriftDetectionStatus where
    toText (StackSetDriftDetectionStatus' ci) = original ci

-- | Represents an enum of /known/ $StackSetDriftDetectionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StackSetDriftDetectionStatus where
    toEnum i = case i of
        0 -> SSDDSCompleted
        1 -> SSDDSFailed
        2 -> SSDDSInProgress
        3 -> SSDDSPartialSuccess
        4 -> SSDDSStopped
        _ -> (error . showText) $ "Unknown index for StackSetDriftDetectionStatus: " <> toText i
    fromEnum x = case x of
        SSDDSCompleted -> 0
        SSDDSFailed -> 1
        SSDDSInProgress -> 2
        SSDDSPartialSuccess -> 3
        SSDDSStopped -> 4
        StackSetDriftDetectionStatus' name -> (error . showText) $ "Unknown StackSetDriftDetectionStatus: " <> original name

-- | Represents the bounds of /known/ $StackSetDriftDetectionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StackSetDriftDetectionStatus where
    minBound = SSDDSCompleted
    maxBound = SSDDSStopped

instance Hashable     StackSetDriftDetectionStatus
instance NFData       StackSetDriftDetectionStatus
instance ToByteString StackSetDriftDetectionStatus
instance ToQuery      StackSetDriftDetectionStatus
instance ToHeader     StackSetDriftDetectionStatus

instance FromXML StackSetDriftDetectionStatus where
    parseXML = parseXMLText "StackSetDriftDetectionStatus"
