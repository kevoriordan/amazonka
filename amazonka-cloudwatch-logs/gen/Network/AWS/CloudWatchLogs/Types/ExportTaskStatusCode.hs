{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.ExportTaskStatusCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchLogs.Types.ExportTaskStatusCode (
  ExportTaskStatusCode (
    ..
    , ETSCCancelled
    , ETSCCompleted
    , ETSCFailed
    , ETSCPending
    , ETSCPendingCancel
    , ETSCRunning
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ExportTaskStatusCode = ExportTaskStatusCode' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern ETSCCancelled :: ExportTaskStatusCode
pattern ETSCCancelled = ExportTaskStatusCode' "CANCELLED"

pattern ETSCCompleted :: ExportTaskStatusCode
pattern ETSCCompleted = ExportTaskStatusCode' "COMPLETED"

pattern ETSCFailed :: ExportTaskStatusCode
pattern ETSCFailed = ExportTaskStatusCode' "FAILED"

pattern ETSCPending :: ExportTaskStatusCode
pattern ETSCPending = ExportTaskStatusCode' "PENDING"

pattern ETSCPendingCancel :: ExportTaskStatusCode
pattern ETSCPendingCancel = ExportTaskStatusCode' "PENDING_CANCEL"

pattern ETSCRunning :: ExportTaskStatusCode
pattern ETSCRunning = ExportTaskStatusCode' "RUNNING"

{-# COMPLETE
  ETSCCancelled,
  ETSCCompleted,
  ETSCFailed,
  ETSCPending,
  ETSCPendingCancel,
  ETSCRunning,
  ExportTaskStatusCode' #-}

instance FromText ExportTaskStatusCode where
    parser = (ExportTaskStatusCode' . mk) <$> takeText

instance ToText ExportTaskStatusCode where
    toText (ExportTaskStatusCode' ci) = original ci

-- | Represents an enum of /known/ $ExportTaskStatusCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ExportTaskStatusCode where
    toEnum i = case i of
        0 -> ETSCCancelled
        1 -> ETSCCompleted
        2 -> ETSCFailed
        3 -> ETSCPending
        4 -> ETSCPendingCancel
        5 -> ETSCRunning
        _ -> (error . showText) $ "Unknown index for ExportTaskStatusCode: " <> toText i
    fromEnum x = case x of
        ETSCCancelled -> 0
        ETSCCompleted -> 1
        ETSCFailed -> 2
        ETSCPending -> 3
        ETSCPendingCancel -> 4
        ETSCRunning -> 5
        ExportTaskStatusCode' name -> (error . showText) $ "Unknown ExportTaskStatusCode: " <> original name

-- | Represents the bounds of /known/ $ExportTaskStatusCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ExportTaskStatusCode where
    minBound = ETSCCancelled
    maxBound = ETSCRunning

instance Hashable     ExportTaskStatusCode
instance NFData       ExportTaskStatusCode
instance ToByteString ExportTaskStatusCode
instance ToQuery      ExportTaskStatusCode
instance ToHeader     ExportTaskStatusCode

instance ToJSON ExportTaskStatusCode where
    toJSON = toJSONText

instance FromJSON ExportTaskStatusCode where
    parseJSON = parseJSONText "ExportTaskStatusCode"
