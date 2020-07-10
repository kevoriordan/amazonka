{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditMitigationActionsTaskStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AuditMitigationActionsTaskStatus (
  AuditMitigationActionsTaskStatus (
    ..
    , Canceled
    , Completed
    , Failed
    , InProgress
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AuditMitigationActionsTaskStatus = AuditMitigationActionsTaskStatus' (CI
                                                                             Text)
                                          deriving (Eq, Ord, Read, Show, Data,
                                                    Typeable, Generic)

pattern Canceled :: AuditMitigationActionsTaskStatus
pattern Canceled = AuditMitigationActionsTaskStatus' "CANCELED"

pattern Completed :: AuditMitigationActionsTaskStatus
pattern Completed = AuditMitigationActionsTaskStatus' "COMPLETED"

pattern Failed :: AuditMitigationActionsTaskStatus
pattern Failed = AuditMitigationActionsTaskStatus' "FAILED"

pattern InProgress :: AuditMitigationActionsTaskStatus
pattern InProgress = AuditMitigationActionsTaskStatus' "IN_PROGRESS"

{-# COMPLETE
  Canceled,
  Completed,
  Failed,
  InProgress,
  AuditMitigationActionsTaskStatus' #-}

instance FromText AuditMitigationActionsTaskStatus where
    parser = (AuditMitigationActionsTaskStatus' . mk) <$> takeText

instance ToText AuditMitigationActionsTaskStatus where
    toText (AuditMitigationActionsTaskStatus' ci) = original ci

-- | Represents an enum of /known/ $AuditMitigationActionsTaskStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AuditMitigationActionsTaskStatus where
    toEnum i = case i of
        0 -> Canceled
        1 -> Completed
        2 -> Failed
        3 -> InProgress
        _ -> (error . showText) $ "Unknown index for AuditMitigationActionsTaskStatus: " <> toText i
    fromEnum x = case x of
        Canceled -> 0
        Completed -> 1
        Failed -> 2
        InProgress -> 3
        AuditMitigationActionsTaskStatus' name -> (error . showText) $ "Unknown AuditMitigationActionsTaskStatus: " <> original name

-- | Represents the bounds of /known/ $AuditMitigationActionsTaskStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AuditMitigationActionsTaskStatus where
    minBound = Canceled
    maxBound = InProgress

instance Hashable     AuditMitigationActionsTaskStatus
instance NFData       AuditMitigationActionsTaskStatus
instance ToByteString AuditMitigationActionsTaskStatus
instance ToQuery      AuditMitigationActionsTaskStatus
instance ToHeader     AuditMitigationActionsTaskStatus

instance ToJSON AuditMitigationActionsTaskStatus where
    toJSON = toJSONText

instance FromJSON AuditMitigationActionsTaskStatus where
    parseJSON = parseJSONText "AuditMitigationActionsTaskStatus"
