{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingJobStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ProcessingJobStatus (
  ProcessingJobStatus (
    ..
    , PJSCompleted
    , PJSFailed
    , PJSInProgress
    , PJSStopped
    , PJSStopping
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ProcessingJobStatus = ProcessingJobStatus' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern PJSCompleted :: ProcessingJobStatus
pattern PJSCompleted = ProcessingJobStatus' "Completed"

pattern PJSFailed :: ProcessingJobStatus
pattern PJSFailed = ProcessingJobStatus' "Failed"

pattern PJSInProgress :: ProcessingJobStatus
pattern PJSInProgress = ProcessingJobStatus' "InProgress"

pattern PJSStopped :: ProcessingJobStatus
pattern PJSStopped = ProcessingJobStatus' "Stopped"

pattern PJSStopping :: ProcessingJobStatus
pattern PJSStopping = ProcessingJobStatus' "Stopping"

{-# COMPLETE
  PJSCompleted,
  PJSFailed,
  PJSInProgress,
  PJSStopped,
  PJSStopping,
  ProcessingJobStatus' #-}

instance FromText ProcessingJobStatus where
    parser = (ProcessingJobStatus' . mk) <$> takeText

instance ToText ProcessingJobStatus where
    toText (ProcessingJobStatus' ci) = original ci

-- | Represents an enum of /known/ $ProcessingJobStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProcessingJobStatus where
    toEnum i = case i of
        0 -> PJSCompleted
        1 -> PJSFailed
        2 -> PJSInProgress
        3 -> PJSStopped
        4 -> PJSStopping
        _ -> (error . showText) $ "Unknown index for ProcessingJobStatus: " <> toText i
    fromEnum x = case x of
        PJSCompleted -> 0
        PJSFailed -> 1
        PJSInProgress -> 2
        PJSStopped -> 3
        PJSStopping -> 4
        ProcessingJobStatus' name -> (error . showText) $ "Unknown ProcessingJobStatus: " <> original name

-- | Represents the bounds of /known/ $ProcessingJobStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProcessingJobStatus where
    minBound = PJSCompleted
    maxBound = PJSStopping

instance Hashable     ProcessingJobStatus
instance NFData       ProcessingJobStatus
instance ToByteString ProcessingJobStatus
instance ToQuery      ProcessingJobStatus
instance ToHeader     ProcessingJobStatus

instance ToJSON ProcessingJobStatus where
    toJSON = toJSONText

instance FromJSON ProcessingJobStatus where
    parseJSON = parseJSONText "ProcessingJobStatus"
