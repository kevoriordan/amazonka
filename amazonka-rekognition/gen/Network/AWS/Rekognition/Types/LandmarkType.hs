{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.LandmarkType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.LandmarkType (
  LandmarkType (
    ..
    , ChinBottom
    , EyeLeft
    , EyeRight
    , LeftEyeBrowLeft
    , LeftEyeBrowRight
    , LeftEyeBrowUp
    , LeftEyeDown
    , LeftEyeLeft
    , LeftEyeRight
    , LeftEyeUp
    , LeftPupil
    , MidJawlineLeft
    , MidJawlineRight
    , MouthDown
    , MouthLeft
    , MouthRight
    , MouthUp
    , Nose
    , NoseLeft
    , NoseRight
    , RightEyeBrowLeft
    , RightEyeBrowRight
    , RightEyeBrowUp
    , RightEyeDown
    , RightEyeLeft
    , RightEyeRight
    , RightEyeUp
    , RightPupil
    , UpperJawlineLeft
    , UpperJawlineRight
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LandmarkType = LandmarkType' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern ChinBottom :: LandmarkType
pattern ChinBottom = LandmarkType' "chinBottom"

pattern EyeLeft :: LandmarkType
pattern EyeLeft = LandmarkType' "eyeLeft"

pattern EyeRight :: LandmarkType
pattern EyeRight = LandmarkType' "eyeRight"

pattern LeftEyeBrowLeft :: LandmarkType
pattern LeftEyeBrowLeft = LandmarkType' "leftEyeBrowLeft"

pattern LeftEyeBrowRight :: LandmarkType
pattern LeftEyeBrowRight = LandmarkType' "leftEyeBrowRight"

pattern LeftEyeBrowUp :: LandmarkType
pattern LeftEyeBrowUp = LandmarkType' "leftEyeBrowUp"

pattern LeftEyeDown :: LandmarkType
pattern LeftEyeDown = LandmarkType' "leftEyeDown"

pattern LeftEyeLeft :: LandmarkType
pattern LeftEyeLeft = LandmarkType' "leftEyeLeft"

pattern LeftEyeRight :: LandmarkType
pattern LeftEyeRight = LandmarkType' "leftEyeRight"

pattern LeftEyeUp :: LandmarkType
pattern LeftEyeUp = LandmarkType' "leftEyeUp"

pattern LeftPupil :: LandmarkType
pattern LeftPupil = LandmarkType' "leftPupil"

pattern MidJawlineLeft :: LandmarkType
pattern MidJawlineLeft = LandmarkType' "midJawlineLeft"

pattern MidJawlineRight :: LandmarkType
pattern MidJawlineRight = LandmarkType' "midJawlineRight"

pattern MouthDown :: LandmarkType
pattern MouthDown = LandmarkType' "mouthDown"

pattern MouthLeft :: LandmarkType
pattern MouthLeft = LandmarkType' "mouthLeft"

pattern MouthRight :: LandmarkType
pattern MouthRight = LandmarkType' "mouthRight"

pattern MouthUp :: LandmarkType
pattern MouthUp = LandmarkType' "mouthUp"

pattern Nose :: LandmarkType
pattern Nose = LandmarkType' "nose"

pattern NoseLeft :: LandmarkType
pattern NoseLeft = LandmarkType' "noseLeft"

pattern NoseRight :: LandmarkType
pattern NoseRight = LandmarkType' "noseRight"

pattern RightEyeBrowLeft :: LandmarkType
pattern RightEyeBrowLeft = LandmarkType' "rightEyeBrowLeft"

pattern RightEyeBrowRight :: LandmarkType
pattern RightEyeBrowRight = LandmarkType' "rightEyeBrowRight"

pattern RightEyeBrowUp :: LandmarkType
pattern RightEyeBrowUp = LandmarkType' "rightEyeBrowUp"

pattern RightEyeDown :: LandmarkType
pattern RightEyeDown = LandmarkType' "rightEyeDown"

pattern RightEyeLeft :: LandmarkType
pattern RightEyeLeft = LandmarkType' "rightEyeLeft"

pattern RightEyeRight :: LandmarkType
pattern RightEyeRight = LandmarkType' "rightEyeRight"

pattern RightEyeUp :: LandmarkType
pattern RightEyeUp = LandmarkType' "rightEyeUp"

pattern RightPupil :: LandmarkType
pattern RightPupil = LandmarkType' "rightPupil"

pattern UpperJawlineLeft :: LandmarkType
pattern UpperJawlineLeft = LandmarkType' "upperJawlineLeft"

pattern UpperJawlineRight :: LandmarkType
pattern UpperJawlineRight = LandmarkType' "upperJawlineRight"

{-# COMPLETE
  ChinBottom,
  EyeLeft,
  EyeRight,
  LeftEyeBrowLeft,
  LeftEyeBrowRight,
  LeftEyeBrowUp,
  LeftEyeDown,
  LeftEyeLeft,
  LeftEyeRight,
  LeftEyeUp,
  LeftPupil,
  MidJawlineLeft,
  MidJawlineRight,
  MouthDown,
  MouthLeft,
  MouthRight,
  MouthUp,
  Nose,
  NoseLeft,
  NoseRight,
  RightEyeBrowLeft,
  RightEyeBrowRight,
  RightEyeBrowUp,
  RightEyeDown,
  RightEyeLeft,
  RightEyeRight,
  RightEyeUp,
  RightPupil,
  UpperJawlineLeft,
  UpperJawlineRight,
  LandmarkType' #-}

instance FromText LandmarkType where
    parser = (LandmarkType' . mk) <$> takeText

instance ToText LandmarkType where
    toText (LandmarkType' ci) = original ci

-- | Represents an enum of /known/ $LandmarkType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LandmarkType where
    toEnum i = case i of
        0 -> ChinBottom
        1 -> EyeLeft
        2 -> EyeRight
        3 -> LeftEyeBrowLeft
        4 -> LeftEyeBrowRight
        5 -> LeftEyeBrowUp
        6 -> LeftEyeDown
        7 -> LeftEyeLeft
        8 -> LeftEyeRight
        9 -> LeftEyeUp
        10 -> LeftPupil
        11 -> MidJawlineLeft
        12 -> MidJawlineRight
        13 -> MouthDown
        14 -> MouthLeft
        15 -> MouthRight
        16 -> MouthUp
        17 -> Nose
        18 -> NoseLeft
        19 -> NoseRight
        20 -> RightEyeBrowLeft
        21 -> RightEyeBrowRight
        22 -> RightEyeBrowUp
        23 -> RightEyeDown
        24 -> RightEyeLeft
        25 -> RightEyeRight
        26 -> RightEyeUp
        27 -> RightPupil
        28 -> UpperJawlineLeft
        29 -> UpperJawlineRight
        _ -> (error . showText) $ "Unknown index for LandmarkType: " <> toText i
    fromEnum x = case x of
        ChinBottom -> 0
        EyeLeft -> 1
        EyeRight -> 2
        LeftEyeBrowLeft -> 3
        LeftEyeBrowRight -> 4
        LeftEyeBrowUp -> 5
        LeftEyeDown -> 6
        LeftEyeLeft -> 7
        LeftEyeRight -> 8
        LeftEyeUp -> 9
        LeftPupil -> 10
        MidJawlineLeft -> 11
        MidJawlineRight -> 12
        MouthDown -> 13
        MouthLeft -> 14
        MouthRight -> 15
        MouthUp -> 16
        Nose -> 17
        NoseLeft -> 18
        NoseRight -> 19
        RightEyeBrowLeft -> 20
        RightEyeBrowRight -> 21
        RightEyeBrowUp -> 22
        RightEyeDown -> 23
        RightEyeLeft -> 24
        RightEyeRight -> 25
        RightEyeUp -> 26
        RightPupil -> 27
        UpperJawlineLeft -> 28
        UpperJawlineRight -> 29
        LandmarkType' name -> (error . showText) $ "Unknown LandmarkType: " <> original name

-- | Represents the bounds of /known/ $LandmarkType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LandmarkType where
    minBound = ChinBottom
    maxBound = UpperJawlineRight

instance Hashable     LandmarkType
instance NFData       LandmarkType
instance ToByteString LandmarkType
instance ToQuery      LandmarkType
instance ToHeader     LandmarkType

instance FromJSON LandmarkType where
    parseJSON = parseJSONText "LandmarkType"
