{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265CodecLevel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H265CodecLevel (
  H265CodecLevel (
    ..
    , HCLAuto
    , HCLLevel1
    , HCLLevel2
    , HCLLevel21
    , HCLLevel3
    , HCLLevel31
    , HCLLevel4
    , HCLLevel41
    , HCLLevel5
    , HCLLevel51
    , HCLLevel52
    , HCLLevel6
    , HCLLevel61
    , HCLLevel62
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | H.265 Level.
data H265CodecLevel = H265CodecLevel' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern HCLAuto :: H265CodecLevel
pattern HCLAuto = H265CodecLevel' "AUTO"

pattern HCLLevel1 :: H265CodecLevel
pattern HCLLevel1 = H265CodecLevel' "LEVEL_1"

pattern HCLLevel2 :: H265CodecLevel
pattern HCLLevel2 = H265CodecLevel' "LEVEL_2"

pattern HCLLevel21 :: H265CodecLevel
pattern HCLLevel21 = H265CodecLevel' "LEVEL_2_1"

pattern HCLLevel3 :: H265CodecLevel
pattern HCLLevel3 = H265CodecLevel' "LEVEL_3"

pattern HCLLevel31 :: H265CodecLevel
pattern HCLLevel31 = H265CodecLevel' "LEVEL_3_1"

pattern HCLLevel4 :: H265CodecLevel
pattern HCLLevel4 = H265CodecLevel' "LEVEL_4"

pattern HCLLevel41 :: H265CodecLevel
pattern HCLLevel41 = H265CodecLevel' "LEVEL_4_1"

pattern HCLLevel5 :: H265CodecLevel
pattern HCLLevel5 = H265CodecLevel' "LEVEL_5"

pattern HCLLevel51 :: H265CodecLevel
pattern HCLLevel51 = H265CodecLevel' "LEVEL_5_1"

pattern HCLLevel52 :: H265CodecLevel
pattern HCLLevel52 = H265CodecLevel' "LEVEL_5_2"

pattern HCLLevel6 :: H265CodecLevel
pattern HCLLevel6 = H265CodecLevel' "LEVEL_6"

pattern HCLLevel61 :: H265CodecLevel
pattern HCLLevel61 = H265CodecLevel' "LEVEL_6_1"

pattern HCLLevel62 :: H265CodecLevel
pattern HCLLevel62 = H265CodecLevel' "LEVEL_6_2"

{-# COMPLETE
  HCLAuto,
  HCLLevel1,
  HCLLevel2,
  HCLLevel21,
  HCLLevel3,
  HCLLevel31,
  HCLLevel4,
  HCLLevel41,
  HCLLevel5,
  HCLLevel51,
  HCLLevel52,
  HCLLevel6,
  HCLLevel61,
  HCLLevel62,
  H265CodecLevel' #-}

instance FromText H265CodecLevel where
    parser = (H265CodecLevel' . mk) <$> takeText

instance ToText H265CodecLevel where
    toText (H265CodecLevel' ci) = original ci

-- | Represents an enum of /known/ $H265CodecLevel.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H265CodecLevel where
    toEnum i = case i of
        0 -> HCLAuto
        1 -> HCLLevel1
        2 -> HCLLevel2
        3 -> HCLLevel21
        4 -> HCLLevel3
        5 -> HCLLevel31
        6 -> HCLLevel4
        7 -> HCLLevel41
        8 -> HCLLevel5
        9 -> HCLLevel51
        10 -> HCLLevel52
        11 -> HCLLevel6
        12 -> HCLLevel61
        13 -> HCLLevel62
        _ -> (error . showText) $ "Unknown index for H265CodecLevel: " <> toText i
    fromEnum x = case x of
        HCLAuto -> 0
        HCLLevel1 -> 1
        HCLLevel2 -> 2
        HCLLevel21 -> 3
        HCLLevel3 -> 4
        HCLLevel31 -> 5
        HCLLevel4 -> 6
        HCLLevel41 -> 7
        HCLLevel5 -> 8
        HCLLevel51 -> 9
        HCLLevel52 -> 10
        HCLLevel6 -> 11
        HCLLevel61 -> 12
        HCLLevel62 -> 13
        H265CodecLevel' name -> (error . showText) $ "Unknown H265CodecLevel: " <> original name

-- | Represents the bounds of /known/ $H265CodecLevel.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H265CodecLevel where
    minBound = HCLAuto
    maxBound = HCLLevel62

instance Hashable     H265CodecLevel
instance NFData       H265CodecLevel
instance ToByteString H265CodecLevel
instance ToQuery      H265CodecLevel
instance ToHeader     H265CodecLevel

instance ToJSON H265CodecLevel where
    toJSON = toJSONText

instance FromJSON H265CodecLevel where
    parseJSON = parseJSONText "H265CodecLevel"
