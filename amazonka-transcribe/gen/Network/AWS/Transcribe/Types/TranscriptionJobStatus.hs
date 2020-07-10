{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.TranscriptionJobStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Transcribe.Types.TranscriptionJobStatus (
  TranscriptionJobStatus (
    ..
    , TJSCompleted
    , TJSFailed
    , TJSInProgress
    , TJSQueued
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TranscriptionJobStatus = TranscriptionJobStatus' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern TJSCompleted :: TranscriptionJobStatus
pattern TJSCompleted = TranscriptionJobStatus' "COMPLETED"

pattern TJSFailed :: TranscriptionJobStatus
pattern TJSFailed = TranscriptionJobStatus' "FAILED"

pattern TJSInProgress :: TranscriptionJobStatus
pattern TJSInProgress = TranscriptionJobStatus' "IN_PROGRESS"

pattern TJSQueued :: TranscriptionJobStatus
pattern TJSQueued = TranscriptionJobStatus' "QUEUED"

{-# COMPLETE
  TJSCompleted,
  TJSFailed,
  TJSInProgress,
  TJSQueued,
  TranscriptionJobStatus' #-}

instance FromText TranscriptionJobStatus where
    parser = (TranscriptionJobStatus' . mk) <$> takeText

instance ToText TranscriptionJobStatus where
    toText (TranscriptionJobStatus' ci) = original ci

-- | Represents an enum of /known/ $TranscriptionJobStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TranscriptionJobStatus where
    toEnum i = case i of
        0 -> TJSCompleted
        1 -> TJSFailed
        2 -> TJSInProgress
        3 -> TJSQueued
        _ -> (error . showText) $ "Unknown index for TranscriptionJobStatus: " <> toText i
    fromEnum x = case x of
        TJSCompleted -> 0
        TJSFailed -> 1
        TJSInProgress -> 2
        TJSQueued -> 3
        TranscriptionJobStatus' name -> (error . showText) $ "Unknown TranscriptionJobStatus: " <> original name

-- | Represents the bounds of /known/ $TranscriptionJobStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TranscriptionJobStatus where
    minBound = TJSCompleted
    maxBound = TJSQueued

instance Hashable     TranscriptionJobStatus
instance NFData       TranscriptionJobStatus
instance ToByteString TranscriptionJobStatus
instance ToQuery      TranscriptionJobStatus
instance ToHeader     TranscriptionJobStatus

instance ToJSON TranscriptionJobStatus where
    toJSON = toJSONText

instance FromJSON TranscriptionJobStatus where
    parseJSON = parseJSONText "TranscriptionJobStatus"
