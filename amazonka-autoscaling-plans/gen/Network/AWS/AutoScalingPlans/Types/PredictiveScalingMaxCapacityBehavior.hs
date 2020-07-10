{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.PredictiveScalingMaxCapacityBehavior
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScalingPlans.Types.PredictiveScalingMaxCapacityBehavior (
  PredictiveScalingMaxCapacityBehavior (
    ..
    , SetForecastCapacityToMaxCapacity
    , SetMaxCapacityAboveForecastCapacity
    , SetMaxCapacityToForecastCapacity
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PredictiveScalingMaxCapacityBehavior = PredictiveScalingMaxCapacityBehavior' (CI
                                                                                     Text)
                                              deriving (Eq, Ord, Read, Show,
                                                        Data, Typeable, Generic)

pattern SetForecastCapacityToMaxCapacity :: PredictiveScalingMaxCapacityBehavior
pattern SetForecastCapacityToMaxCapacity = PredictiveScalingMaxCapacityBehavior' "SetForecastCapacityToMaxCapacity"

pattern SetMaxCapacityAboveForecastCapacity :: PredictiveScalingMaxCapacityBehavior
pattern SetMaxCapacityAboveForecastCapacity = PredictiveScalingMaxCapacityBehavior' "SetMaxCapacityAboveForecastCapacity"

pattern SetMaxCapacityToForecastCapacity :: PredictiveScalingMaxCapacityBehavior
pattern SetMaxCapacityToForecastCapacity = PredictiveScalingMaxCapacityBehavior' "SetMaxCapacityToForecastCapacity"

{-# COMPLETE
  SetForecastCapacityToMaxCapacity,
  SetMaxCapacityAboveForecastCapacity,
  SetMaxCapacityToForecastCapacity,
  PredictiveScalingMaxCapacityBehavior' #-}

instance FromText PredictiveScalingMaxCapacityBehavior where
    parser = (PredictiveScalingMaxCapacityBehavior' . mk) <$> takeText

instance ToText PredictiveScalingMaxCapacityBehavior where
    toText (PredictiveScalingMaxCapacityBehavior' ci) = original ci

-- | Represents an enum of /known/ $PredictiveScalingMaxCapacityBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PredictiveScalingMaxCapacityBehavior where
    toEnum i = case i of
        0 -> SetForecastCapacityToMaxCapacity
        1 -> SetMaxCapacityAboveForecastCapacity
        2 -> SetMaxCapacityToForecastCapacity
        _ -> (error . showText) $ "Unknown index for PredictiveScalingMaxCapacityBehavior: " <> toText i
    fromEnum x = case x of
        SetForecastCapacityToMaxCapacity -> 0
        SetMaxCapacityAboveForecastCapacity -> 1
        SetMaxCapacityToForecastCapacity -> 2
        PredictiveScalingMaxCapacityBehavior' name -> (error . showText) $ "Unknown PredictiveScalingMaxCapacityBehavior: " <> original name

-- | Represents the bounds of /known/ $PredictiveScalingMaxCapacityBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PredictiveScalingMaxCapacityBehavior where
    minBound = SetForecastCapacityToMaxCapacity
    maxBound = SetMaxCapacityToForecastCapacity

instance Hashable     PredictiveScalingMaxCapacityBehavior
instance NFData       PredictiveScalingMaxCapacityBehavior
instance ToByteString PredictiveScalingMaxCapacityBehavior
instance ToQuery      PredictiveScalingMaxCapacityBehavior
instance ToHeader     PredictiveScalingMaxCapacityBehavior

instance ToJSON PredictiveScalingMaxCapacityBehavior where
    toJSON = toJSONText

instance FromJSON PredictiveScalingMaxCapacityBehavior where
    parseJSON = parseJSONText "PredictiveScalingMaxCapacityBehavior"
