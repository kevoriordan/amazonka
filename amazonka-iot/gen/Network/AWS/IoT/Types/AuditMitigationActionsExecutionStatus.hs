{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditMitigationActionsExecutionStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AuditMitigationActionsExecutionStatus (
  AuditMitigationActionsExecutionStatus (
    ..
    , AMAESCanceled
    , AMAESCompleted
    , AMAESFailed
    , AMAESInProgress
    , AMAESPending
    , AMAESSkipped
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AuditMitigationActionsExecutionStatus = AuditMitigationActionsExecutionStatus' (CI
                                                                                       Text)
                                               deriving (Eq, Ord, Read, Show,
                                                         Data, Typeable,
                                                         Generic)

pattern AMAESCanceled :: AuditMitigationActionsExecutionStatus
pattern AMAESCanceled = AuditMitigationActionsExecutionStatus' "CANCELED"

pattern AMAESCompleted :: AuditMitigationActionsExecutionStatus
pattern AMAESCompleted = AuditMitigationActionsExecutionStatus' "COMPLETED"

pattern AMAESFailed :: AuditMitigationActionsExecutionStatus
pattern AMAESFailed = AuditMitigationActionsExecutionStatus' "FAILED"

pattern AMAESInProgress :: AuditMitigationActionsExecutionStatus
pattern AMAESInProgress = AuditMitigationActionsExecutionStatus' "IN_PROGRESS"

pattern AMAESPending :: AuditMitigationActionsExecutionStatus
pattern AMAESPending = AuditMitigationActionsExecutionStatus' "PENDING"

pattern AMAESSkipped :: AuditMitigationActionsExecutionStatus
pattern AMAESSkipped = AuditMitigationActionsExecutionStatus' "SKIPPED"

{-# COMPLETE
  AMAESCanceled,
  AMAESCompleted,
  AMAESFailed,
  AMAESInProgress,
  AMAESPending,
  AMAESSkipped,
  AuditMitigationActionsExecutionStatus' #-}

instance FromText AuditMitigationActionsExecutionStatus where
    parser = (AuditMitigationActionsExecutionStatus' . mk) <$> takeText

instance ToText AuditMitigationActionsExecutionStatus where
    toText (AuditMitigationActionsExecutionStatus' ci) = original ci

-- | Represents an enum of /known/ $AuditMitigationActionsExecutionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AuditMitigationActionsExecutionStatus where
    toEnum i = case i of
        0 -> AMAESCanceled
        1 -> AMAESCompleted
        2 -> AMAESFailed
        3 -> AMAESInProgress
        4 -> AMAESPending
        5 -> AMAESSkipped
        _ -> (error . showText) $ "Unknown index for AuditMitigationActionsExecutionStatus: " <> toText i
    fromEnum x = case x of
        AMAESCanceled -> 0
        AMAESCompleted -> 1
        AMAESFailed -> 2
        AMAESInProgress -> 3
        AMAESPending -> 4
        AMAESSkipped -> 5
        AuditMitigationActionsExecutionStatus' name -> (error . showText) $ "Unknown AuditMitigationActionsExecutionStatus: " <> original name

-- | Represents the bounds of /known/ $AuditMitigationActionsExecutionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AuditMitigationActionsExecutionStatus where
    minBound = AMAESCanceled
    maxBound = AMAESSkipped

instance Hashable     AuditMitigationActionsExecutionStatus
instance NFData       AuditMitigationActionsExecutionStatus
instance ToByteString AuditMitigationActionsExecutionStatus
instance ToQuery      AuditMitigationActionsExecutionStatus
instance ToHeader     AuditMitigationActionsExecutionStatus

instance ToJSON AuditMitigationActionsExecutionStatus where
    toJSON = toJSONText

instance FromJSON AuditMitigationActionsExecutionStatus where
    parseJSON = parseJSONText "AuditMitigationActionsExecutionStatus"
