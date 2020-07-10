{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264CodecLevel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H264CodecLevel (
  H264CodecLevel (
    ..
    , HAuto
    , HLevel1
    , HLevel11
    , HLevel12
    , HLevel13
    , HLevel2
    , HLevel21
    , HLevel22
    , HLevel3
    , HLevel31
    , HLevel32
    , HLevel4
    , HLevel41
    , HLevel42
    , HLevel5
    , HLevel51
    , HLevel52
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Specify an H.264 level that is consistent with your output video settings. If you aren't sure what level to specify, choose Auto (AUTO).
data H264CodecLevel = H264CodecLevel' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern HAuto :: H264CodecLevel
pattern HAuto = H264CodecLevel' "AUTO"

pattern HLevel1 :: H264CodecLevel
pattern HLevel1 = H264CodecLevel' "LEVEL_1"

pattern HLevel11 :: H264CodecLevel
pattern HLevel11 = H264CodecLevel' "LEVEL_1_1"

pattern HLevel12 :: H264CodecLevel
pattern HLevel12 = H264CodecLevel' "LEVEL_1_2"

pattern HLevel13 :: H264CodecLevel
pattern HLevel13 = H264CodecLevel' "LEVEL_1_3"

pattern HLevel2 :: H264CodecLevel
pattern HLevel2 = H264CodecLevel' "LEVEL_2"

pattern HLevel21 :: H264CodecLevel
pattern HLevel21 = H264CodecLevel' "LEVEL_2_1"

pattern HLevel22 :: H264CodecLevel
pattern HLevel22 = H264CodecLevel' "LEVEL_2_2"

pattern HLevel3 :: H264CodecLevel
pattern HLevel3 = H264CodecLevel' "LEVEL_3"

pattern HLevel31 :: H264CodecLevel
pattern HLevel31 = H264CodecLevel' "LEVEL_3_1"

pattern HLevel32 :: H264CodecLevel
pattern HLevel32 = H264CodecLevel' "LEVEL_3_2"

pattern HLevel4 :: H264CodecLevel
pattern HLevel4 = H264CodecLevel' "LEVEL_4"

pattern HLevel41 :: H264CodecLevel
pattern HLevel41 = H264CodecLevel' "LEVEL_4_1"

pattern HLevel42 :: H264CodecLevel
pattern HLevel42 = H264CodecLevel' "LEVEL_4_2"

pattern HLevel5 :: H264CodecLevel
pattern HLevel5 = H264CodecLevel' "LEVEL_5"

pattern HLevel51 :: H264CodecLevel
pattern HLevel51 = H264CodecLevel' "LEVEL_5_1"

pattern HLevel52 :: H264CodecLevel
pattern HLevel52 = H264CodecLevel' "LEVEL_5_2"

{-# COMPLETE
  HAuto,
  HLevel1,
  HLevel11,
  HLevel12,
  HLevel13,
  HLevel2,
  HLevel21,
  HLevel22,
  HLevel3,
  HLevel31,
  HLevel32,
  HLevel4,
  HLevel41,
  HLevel42,
  HLevel5,
  HLevel51,
  HLevel52,
  H264CodecLevel' #-}

instance FromText H264CodecLevel where
    parser = (H264CodecLevel' . mk) <$> takeText

instance ToText H264CodecLevel where
    toText (H264CodecLevel' ci) = original ci

-- | Represents an enum of /known/ $H264CodecLevel.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H264CodecLevel where
    toEnum i = case i of
        0 -> HAuto
        1 -> HLevel1
        2 -> HLevel11
        3 -> HLevel12
        4 -> HLevel13
        5 -> HLevel2
        6 -> HLevel21
        7 -> HLevel22
        8 -> HLevel3
        9 -> HLevel31
        10 -> HLevel32
        11 -> HLevel4
        12 -> HLevel41
        13 -> HLevel42
        14 -> HLevel5
        15 -> HLevel51
        16 -> HLevel52
        _ -> (error . showText) $ "Unknown index for H264CodecLevel: " <> toText i
    fromEnum x = case x of
        HAuto -> 0
        HLevel1 -> 1
        HLevel11 -> 2
        HLevel12 -> 3
        HLevel13 -> 4
        HLevel2 -> 5
        HLevel21 -> 6
        HLevel22 -> 7
        HLevel3 -> 8
        HLevel31 -> 9
        HLevel32 -> 10
        HLevel4 -> 11
        HLevel41 -> 12
        HLevel42 -> 13
        HLevel5 -> 14
        HLevel51 -> 15
        HLevel52 -> 16
        H264CodecLevel' name -> (error . showText) $ "Unknown H264CodecLevel: " <> original name

-- | Represents the bounds of /known/ $H264CodecLevel.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H264CodecLevel where
    minBound = HAuto
    maxBound = HLevel52

instance Hashable     H264CodecLevel
instance NFData       H264CodecLevel
instance ToByteString H264CodecLevel
instance ToQuery      H264CodecLevel
instance ToHeader     H264CodecLevel

instance ToJSON H264CodecLevel where
    toJSON = toJSONText

instance FromJSON H264CodecLevel where
    parseJSON = parseJSONText "H264CodecLevel"
