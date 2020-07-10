{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DashIsoPlaybackDeviceCompatibility
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.DashIsoPlaybackDeviceCompatibility (
  DashIsoPlaybackDeviceCompatibility (
    ..
    , CencV1
    , UnencryptedSei
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | This setting can improve the compatibility of your output with video players on obsolete devices. It applies only to DASH H.264 outputs with DRM encryption. Choose Unencrypted SEI (UNENCRYPTED_SEI) only to correct problems with playback on older devices. Otherwise, keep the default setting CENC v1 (CENC_V1). If you choose Unencrypted SEI, for that output, the service will exclude the access unit delimiter and will leave the SEI NAL units unencrypted.
data DashIsoPlaybackDeviceCompatibility = DashIsoPlaybackDeviceCompatibility' (CI
                                                                                 Text)
                                            deriving (Eq, Ord, Read, Show, Data,
                                                      Typeable, Generic)

pattern CencV1 :: DashIsoPlaybackDeviceCompatibility
pattern CencV1 = DashIsoPlaybackDeviceCompatibility' "CENC_V1"

pattern UnencryptedSei :: DashIsoPlaybackDeviceCompatibility
pattern UnencryptedSei = DashIsoPlaybackDeviceCompatibility' "UNENCRYPTED_SEI"

{-# COMPLETE
  CencV1,
  UnencryptedSei,
  DashIsoPlaybackDeviceCompatibility' #-}

instance FromText DashIsoPlaybackDeviceCompatibility where
    parser = (DashIsoPlaybackDeviceCompatibility' . mk) <$> takeText

instance ToText DashIsoPlaybackDeviceCompatibility where
    toText (DashIsoPlaybackDeviceCompatibility' ci) = original ci

-- | Represents an enum of /known/ $DashIsoPlaybackDeviceCompatibility.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DashIsoPlaybackDeviceCompatibility where
    toEnum i = case i of
        0 -> CencV1
        1 -> UnencryptedSei
        _ -> (error . showText) $ "Unknown index for DashIsoPlaybackDeviceCompatibility: " <> toText i
    fromEnum x = case x of
        CencV1 -> 0
        UnencryptedSei -> 1
        DashIsoPlaybackDeviceCompatibility' name -> (error . showText) $ "Unknown DashIsoPlaybackDeviceCompatibility: " <> original name

-- | Represents the bounds of /known/ $DashIsoPlaybackDeviceCompatibility.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DashIsoPlaybackDeviceCompatibility where
    minBound = CencV1
    maxBound = UnencryptedSei

instance Hashable     DashIsoPlaybackDeviceCompatibility
instance NFData       DashIsoPlaybackDeviceCompatibility
instance ToByteString DashIsoPlaybackDeviceCompatibility
instance ToQuery      DashIsoPlaybackDeviceCompatibility
instance ToHeader     DashIsoPlaybackDeviceCompatibility

instance ToJSON DashIsoPlaybackDeviceCompatibility where
    toJSON = toJSONText

instance FromJSON DashIsoPlaybackDeviceCompatibility where
    parseJSON = parseJSONText "DashIsoPlaybackDeviceCompatibility"
