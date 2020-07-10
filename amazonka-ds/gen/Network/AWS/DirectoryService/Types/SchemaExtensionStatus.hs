{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.SchemaExtensionStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.SchemaExtensionStatus (
  SchemaExtensionStatus (
    ..
    , SESCancelInProgress
    , SESCancelled
    , SESCompleted
    , SESCreatingSnapshot
    , SESFailed
    , SESInitializing
    , SESReplicating
    , SESRollbackInProgress
    , SESUpdatingSchema
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SchemaExtensionStatus = SchemaExtensionStatus' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern SESCancelInProgress :: SchemaExtensionStatus
pattern SESCancelInProgress = SchemaExtensionStatus' "CancelInProgress"

pattern SESCancelled :: SchemaExtensionStatus
pattern SESCancelled = SchemaExtensionStatus' "Cancelled"

pattern SESCompleted :: SchemaExtensionStatus
pattern SESCompleted = SchemaExtensionStatus' "Completed"

pattern SESCreatingSnapshot :: SchemaExtensionStatus
pattern SESCreatingSnapshot = SchemaExtensionStatus' "CreatingSnapshot"

pattern SESFailed :: SchemaExtensionStatus
pattern SESFailed = SchemaExtensionStatus' "Failed"

pattern SESInitializing :: SchemaExtensionStatus
pattern SESInitializing = SchemaExtensionStatus' "Initializing"

pattern SESReplicating :: SchemaExtensionStatus
pattern SESReplicating = SchemaExtensionStatus' "Replicating"

pattern SESRollbackInProgress :: SchemaExtensionStatus
pattern SESRollbackInProgress = SchemaExtensionStatus' "RollbackInProgress"

pattern SESUpdatingSchema :: SchemaExtensionStatus
pattern SESUpdatingSchema = SchemaExtensionStatus' "UpdatingSchema"

{-# COMPLETE
  SESCancelInProgress,
  SESCancelled,
  SESCompleted,
  SESCreatingSnapshot,
  SESFailed,
  SESInitializing,
  SESReplicating,
  SESRollbackInProgress,
  SESUpdatingSchema,
  SchemaExtensionStatus' #-}

instance FromText SchemaExtensionStatus where
    parser = (SchemaExtensionStatus' . mk) <$> takeText

instance ToText SchemaExtensionStatus where
    toText (SchemaExtensionStatus' ci) = original ci

-- | Represents an enum of /known/ $SchemaExtensionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SchemaExtensionStatus where
    toEnum i = case i of
        0 -> SESCancelInProgress
        1 -> SESCancelled
        2 -> SESCompleted
        3 -> SESCreatingSnapshot
        4 -> SESFailed
        5 -> SESInitializing
        6 -> SESReplicating
        7 -> SESRollbackInProgress
        8 -> SESUpdatingSchema
        _ -> (error . showText) $ "Unknown index for SchemaExtensionStatus: " <> toText i
    fromEnum x = case x of
        SESCancelInProgress -> 0
        SESCancelled -> 1
        SESCompleted -> 2
        SESCreatingSnapshot -> 3
        SESFailed -> 4
        SESInitializing -> 5
        SESReplicating -> 6
        SESRollbackInProgress -> 7
        SESUpdatingSchema -> 8
        SchemaExtensionStatus' name -> (error . showText) $ "Unknown SchemaExtensionStatus: " <> original name

-- | Represents the bounds of /known/ $SchemaExtensionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SchemaExtensionStatus where
    minBound = SESCancelInProgress
    maxBound = SESUpdatingSchema

instance Hashable     SchemaExtensionStatus
instance NFData       SchemaExtensionStatus
instance ToByteString SchemaExtensionStatus
instance ToQuery      SchemaExtensionStatus
instance ToHeader     SchemaExtensionStatus

instance FromJSON SchemaExtensionStatus where
    parseJSON = parseJSONText "SchemaExtensionStatus"
