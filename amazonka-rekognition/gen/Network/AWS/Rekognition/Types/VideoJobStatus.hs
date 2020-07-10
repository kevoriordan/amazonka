{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.VideoJobStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.VideoJobStatus (
  VideoJobStatus (
    ..
    , VJSFailed
    , VJSInProgress
    , VJSSucceeded
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data VideoJobStatus = VideoJobStatus' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern VJSFailed :: VideoJobStatus
pattern VJSFailed = VideoJobStatus' "FAILED"

pattern VJSInProgress :: VideoJobStatus
pattern VJSInProgress = VideoJobStatus' "IN_PROGRESS"

pattern VJSSucceeded :: VideoJobStatus
pattern VJSSucceeded = VideoJobStatus' "SUCCEEDED"

{-# COMPLETE
  VJSFailed,
  VJSInProgress,
  VJSSucceeded,
  VideoJobStatus' #-}

instance FromText VideoJobStatus where
    parser = (VideoJobStatus' . mk) <$> takeText

instance ToText VideoJobStatus where
    toText (VideoJobStatus' ci) = original ci

-- | Represents an enum of /known/ $VideoJobStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum VideoJobStatus where
    toEnum i = case i of
        0 -> VJSFailed
        1 -> VJSInProgress
        2 -> VJSSucceeded
        _ -> (error . showText) $ "Unknown index for VideoJobStatus: " <> toText i
    fromEnum x = case x of
        VJSFailed -> 0
        VJSInProgress -> 1
        VJSSucceeded -> 2
        VideoJobStatus' name -> (error . showText) $ "Unknown VideoJobStatus: " <> original name

-- | Represents the bounds of /known/ $VideoJobStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded VideoJobStatus where
    minBound = VJSFailed
    maxBound = VJSSucceeded

instance Hashable     VideoJobStatus
instance NFData       VideoJobStatus
instance ToByteString VideoJobStatus
instance ToQuery      VideoJobStatus
instance ToHeader     VideoJobStatus

instance FromJSON VideoJobStatus where
    parseJSON = parseJSONText "VideoJobStatus"
