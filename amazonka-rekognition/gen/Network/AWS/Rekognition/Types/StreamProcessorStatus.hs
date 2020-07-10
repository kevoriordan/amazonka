{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.StreamProcessorStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.StreamProcessorStatus (
  StreamProcessorStatus (
    ..
    , SPSFailed
    , SPSRunning
    , SPSStarting
    , SPSStopped
    , SPSStopping
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StreamProcessorStatus = StreamProcessorStatus' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern SPSFailed :: StreamProcessorStatus
pattern SPSFailed = StreamProcessorStatus' "FAILED"

pattern SPSRunning :: StreamProcessorStatus
pattern SPSRunning = StreamProcessorStatus' "RUNNING"

pattern SPSStarting :: StreamProcessorStatus
pattern SPSStarting = StreamProcessorStatus' "STARTING"

pattern SPSStopped :: StreamProcessorStatus
pattern SPSStopped = StreamProcessorStatus' "STOPPED"

pattern SPSStopping :: StreamProcessorStatus
pattern SPSStopping = StreamProcessorStatus' "STOPPING"

{-# COMPLETE
  SPSFailed,
  SPSRunning,
  SPSStarting,
  SPSStopped,
  SPSStopping,
  StreamProcessorStatus' #-}

instance FromText StreamProcessorStatus where
    parser = (StreamProcessorStatus' . mk) <$> takeText

instance ToText StreamProcessorStatus where
    toText (StreamProcessorStatus' ci) = original ci

-- | Represents an enum of /known/ $StreamProcessorStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StreamProcessorStatus where
    toEnum i = case i of
        0 -> SPSFailed
        1 -> SPSRunning
        2 -> SPSStarting
        3 -> SPSStopped
        4 -> SPSStopping
        _ -> (error . showText) $ "Unknown index for StreamProcessorStatus: " <> toText i
    fromEnum x = case x of
        SPSFailed -> 0
        SPSRunning -> 1
        SPSStarting -> 2
        SPSStopped -> 3
        SPSStopping -> 4
        StreamProcessorStatus' name -> (error . showText) $ "Unknown StreamProcessorStatus: " <> original name

-- | Represents the bounds of /known/ $StreamProcessorStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StreamProcessorStatus where
    minBound = SPSFailed
    maxBound = SPSStopping

instance Hashable     StreamProcessorStatus
instance NFData       StreamProcessorStatus
instance ToByteString StreamProcessorStatus
instance ToQuery      StreamProcessorStatus
instance ToHeader     StreamProcessorStatus

instance FromJSON StreamProcessorStatus where
    parseJSON = parseJSONText "StreamProcessorStatus"
