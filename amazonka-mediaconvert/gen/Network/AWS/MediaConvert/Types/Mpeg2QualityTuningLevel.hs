{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2QualityTuningLevel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Mpeg2QualityTuningLevel (
  Mpeg2QualityTuningLevel (
    ..
    , MQTLMultiPass
    , MQTLSinglePass
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Use Quality tuning level (Mpeg2QualityTuningLevel) to specifiy whether to use single-pass or multipass video encoding.
data Mpeg2QualityTuningLevel = Mpeg2QualityTuningLevel' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern MQTLMultiPass :: Mpeg2QualityTuningLevel
pattern MQTLMultiPass = Mpeg2QualityTuningLevel' "MULTI_PASS"

pattern MQTLSinglePass :: Mpeg2QualityTuningLevel
pattern MQTLSinglePass = Mpeg2QualityTuningLevel' "SINGLE_PASS"

{-# COMPLETE
  MQTLMultiPass,
  MQTLSinglePass,
  Mpeg2QualityTuningLevel' #-}

instance FromText Mpeg2QualityTuningLevel where
    parser = (Mpeg2QualityTuningLevel' . mk) <$> takeText

instance ToText Mpeg2QualityTuningLevel where
    toText (Mpeg2QualityTuningLevel' ci) = original ci

-- | Represents an enum of /known/ $Mpeg2QualityTuningLevel.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Mpeg2QualityTuningLevel where
    toEnum i = case i of
        0 -> MQTLMultiPass
        1 -> MQTLSinglePass
        _ -> (error . showText) $ "Unknown index for Mpeg2QualityTuningLevel: " <> toText i
    fromEnum x = case x of
        MQTLMultiPass -> 0
        MQTLSinglePass -> 1
        Mpeg2QualityTuningLevel' name -> (error . showText) $ "Unknown Mpeg2QualityTuningLevel: " <> original name

-- | Represents the bounds of /known/ $Mpeg2QualityTuningLevel.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Mpeg2QualityTuningLevel where
    minBound = MQTLMultiPass
    maxBound = MQTLSinglePass

instance Hashable     Mpeg2QualityTuningLevel
instance NFData       Mpeg2QualityTuningLevel
instance ToByteString Mpeg2QualityTuningLevel
instance ToQuery      Mpeg2QualityTuningLevel
instance ToHeader     Mpeg2QualityTuningLevel

instance ToJSON Mpeg2QualityTuningLevel where
    toJSON = toJSONText

instance FromJSON Mpeg2QualityTuningLevel where
    parseJSON = parseJSONText "Mpeg2QualityTuningLevel"
