{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264Level
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H264Level (
  H264Level (
    ..
    , H264Level1
    , H264Level11
    , H264Level12
    , H264Level13
    , H264Level2
    , H264Level21
    , H264Level22
    , H264Level3
    , H264Level31
    , H264Level32
    , H264Level4
    , H264Level41
    , H264Level42
    , H264Level5
    , H264Level51
    , H264Level52
    , H264LevelAuto
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | H264 Level
data H264Level = H264Level' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern H264Level1 :: H264Level
pattern H264Level1 = H264Level' "H264_LEVEL_1"

pattern H264Level11 :: H264Level
pattern H264Level11 = H264Level' "H264_LEVEL_1_1"

pattern H264Level12 :: H264Level
pattern H264Level12 = H264Level' "H264_LEVEL_1_2"

pattern H264Level13 :: H264Level
pattern H264Level13 = H264Level' "H264_LEVEL_1_3"

pattern H264Level2 :: H264Level
pattern H264Level2 = H264Level' "H264_LEVEL_2"

pattern H264Level21 :: H264Level
pattern H264Level21 = H264Level' "H264_LEVEL_2_1"

pattern H264Level22 :: H264Level
pattern H264Level22 = H264Level' "H264_LEVEL_2_2"

pattern H264Level3 :: H264Level
pattern H264Level3 = H264Level' "H264_LEVEL_3"

pattern H264Level31 :: H264Level
pattern H264Level31 = H264Level' "H264_LEVEL_3_1"

pattern H264Level32 :: H264Level
pattern H264Level32 = H264Level' "H264_LEVEL_3_2"

pattern H264Level4 :: H264Level
pattern H264Level4 = H264Level' "H264_LEVEL_4"

pattern H264Level41 :: H264Level
pattern H264Level41 = H264Level' "H264_LEVEL_4_1"

pattern H264Level42 :: H264Level
pattern H264Level42 = H264Level' "H264_LEVEL_4_2"

pattern H264Level5 :: H264Level
pattern H264Level5 = H264Level' "H264_LEVEL_5"

pattern H264Level51 :: H264Level
pattern H264Level51 = H264Level' "H264_LEVEL_5_1"

pattern H264Level52 :: H264Level
pattern H264Level52 = H264Level' "H264_LEVEL_5_2"

pattern H264LevelAuto :: H264Level
pattern H264LevelAuto = H264Level' "H264_LEVEL_AUTO"

{-# COMPLETE
  H264Level1,
  H264Level11,
  H264Level12,
  H264Level13,
  H264Level2,
  H264Level21,
  H264Level22,
  H264Level3,
  H264Level31,
  H264Level32,
  H264Level4,
  H264Level41,
  H264Level42,
  H264Level5,
  H264Level51,
  H264Level52,
  H264LevelAuto,
  H264Level' #-}

instance FromText H264Level where
    parser = (H264Level' . mk) <$> takeText

instance ToText H264Level where
    toText (H264Level' ci) = original ci

-- | Represents an enum of /known/ $H264Level.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H264Level where
    toEnum i = case i of
        0 -> H264Level1
        1 -> H264Level11
        2 -> H264Level12
        3 -> H264Level13
        4 -> H264Level2
        5 -> H264Level21
        6 -> H264Level22
        7 -> H264Level3
        8 -> H264Level31
        9 -> H264Level32
        10 -> H264Level4
        11 -> H264Level41
        12 -> H264Level42
        13 -> H264Level5
        14 -> H264Level51
        15 -> H264Level52
        16 -> H264LevelAuto
        _ -> (error . showText) $ "Unknown index for H264Level: " <> toText i
    fromEnum x = case x of
        H264Level1 -> 0
        H264Level11 -> 1
        H264Level12 -> 2
        H264Level13 -> 3
        H264Level2 -> 4
        H264Level21 -> 5
        H264Level22 -> 6
        H264Level3 -> 7
        H264Level31 -> 8
        H264Level32 -> 9
        H264Level4 -> 10
        H264Level41 -> 11
        H264Level42 -> 12
        H264Level5 -> 13
        H264Level51 -> 14
        H264Level52 -> 15
        H264LevelAuto -> 16
        H264Level' name -> (error . showText) $ "Unknown H264Level: " <> original name

-- | Represents the bounds of /known/ $H264Level.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H264Level where
    minBound = H264Level1
    maxBound = H264LevelAuto

instance Hashable     H264Level
instance NFData       H264Level
instance ToByteString H264Level
instance ToQuery      H264Level
instance ToHeader     H264Level

instance ToJSON H264Level where
    toJSON = toJSONText

instance FromJSON H264Level where
    parseJSON = parseJSONText "H264Level"
