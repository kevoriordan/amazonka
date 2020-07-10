{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265CodecProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H265CodecProfile (
  H265CodecProfile (
    ..
    , MAIN10High
    , MAIN10Main
    , Main42210BITHigh
    , Main42210BITMain
    , Main4228BITHigh
    , Main4228BITMain
    , MainHigh
    , MainMain
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Represents the Profile and Tier, per the HEVC (H.265) specification. Selections are grouped as [Profile] / [Tier], so "Main/High" represents Main Profile with High Tier. 4:2:2 profiles are only available with the HEVC 4:2:2 License.
data H265CodecProfile = H265CodecProfile' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern MAIN10High :: H265CodecProfile
pattern MAIN10High = H265CodecProfile' "MAIN10_HIGH"

pattern MAIN10Main :: H265CodecProfile
pattern MAIN10Main = H265CodecProfile' "MAIN10_MAIN"

pattern Main42210BITHigh :: H265CodecProfile
pattern Main42210BITHigh = H265CodecProfile' "MAIN_422_10BIT_HIGH"

pattern Main42210BITMain :: H265CodecProfile
pattern Main42210BITMain = H265CodecProfile' "MAIN_422_10BIT_MAIN"

pattern Main4228BITHigh :: H265CodecProfile
pattern Main4228BITHigh = H265CodecProfile' "MAIN_422_8BIT_HIGH"

pattern Main4228BITMain :: H265CodecProfile
pattern Main4228BITMain = H265CodecProfile' "MAIN_422_8BIT_MAIN"

pattern MainHigh :: H265CodecProfile
pattern MainHigh = H265CodecProfile' "MAIN_HIGH"

pattern MainMain :: H265CodecProfile
pattern MainMain = H265CodecProfile' "MAIN_MAIN"

{-# COMPLETE
  MAIN10High,
  MAIN10Main,
  Main42210BITHigh,
  Main42210BITMain,
  Main4228BITHigh,
  Main4228BITMain,
  MainHigh,
  MainMain,
  H265CodecProfile' #-}

instance FromText H265CodecProfile where
    parser = (H265CodecProfile' . mk) <$> takeText

instance ToText H265CodecProfile where
    toText (H265CodecProfile' ci) = original ci

-- | Represents an enum of /known/ $H265CodecProfile.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H265CodecProfile where
    toEnum i = case i of
        0 -> MAIN10High
        1 -> MAIN10Main
        2 -> Main42210BITHigh
        3 -> Main42210BITMain
        4 -> Main4228BITHigh
        5 -> Main4228BITMain
        6 -> MainHigh
        7 -> MainMain
        _ -> (error . showText) $ "Unknown index for H265CodecProfile: " <> toText i
    fromEnum x = case x of
        MAIN10High -> 0
        MAIN10Main -> 1
        Main42210BITHigh -> 2
        Main42210BITMain -> 3
        Main4228BITHigh -> 4
        Main4228BITMain -> 5
        MainHigh -> 6
        MainMain -> 7
        H265CodecProfile' name -> (error . showText) $ "Unknown H265CodecProfile: " <> original name

-- | Represents the bounds of /known/ $H265CodecProfile.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H265CodecProfile where
    minBound = MAIN10High
    maxBound = MainMain

instance Hashable     H265CodecProfile
instance NFData       H265CodecProfile
instance ToByteString H265CodecProfile
instance ToQuery      H265CodecProfile
instance ToHeader     H265CodecProfile

instance ToJSON H265CodecProfile where
    toJSON = toJSONText

instance FromJSON H265CodecProfile where
    parseJSON = parseJSONText "H265CodecProfile"
