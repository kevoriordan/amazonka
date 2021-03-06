{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.ScalingAdjustmentType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.ScalingAdjustmentType (
  ScalingAdjustmentType (
    ..
    , ChangeInCapacity
    , ExactCapacity
    , PercentChangeInCapacity
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ScalingAdjustmentType = ScalingAdjustmentType' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern ChangeInCapacity :: ScalingAdjustmentType
pattern ChangeInCapacity = ScalingAdjustmentType' "ChangeInCapacity"

pattern ExactCapacity :: ScalingAdjustmentType
pattern ExactCapacity = ScalingAdjustmentType' "ExactCapacity"

pattern PercentChangeInCapacity :: ScalingAdjustmentType
pattern PercentChangeInCapacity = ScalingAdjustmentType' "PercentChangeInCapacity"

{-# COMPLETE
  ChangeInCapacity,
  ExactCapacity,
  PercentChangeInCapacity,
  ScalingAdjustmentType' #-}

instance FromText ScalingAdjustmentType where
    parser = (ScalingAdjustmentType' . mk) <$> takeText

instance ToText ScalingAdjustmentType where
    toText (ScalingAdjustmentType' ci) = original ci

-- | Represents an enum of /known/ $ScalingAdjustmentType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ScalingAdjustmentType where
    toEnum i = case i of
        0 -> ChangeInCapacity
        1 -> ExactCapacity
        2 -> PercentChangeInCapacity
        _ -> (error . showText) $ "Unknown index for ScalingAdjustmentType: " <> toText i
    fromEnum x = case x of
        ChangeInCapacity -> 0
        ExactCapacity -> 1
        PercentChangeInCapacity -> 2
        ScalingAdjustmentType' name -> (error . showText) $ "Unknown ScalingAdjustmentType: " <> original name

-- | Represents the bounds of /known/ $ScalingAdjustmentType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ScalingAdjustmentType where
    minBound = ChangeInCapacity
    maxBound = PercentChangeInCapacity

instance Hashable     ScalingAdjustmentType
instance NFData       ScalingAdjustmentType
instance ToByteString ScalingAdjustmentType
instance ToQuery      ScalingAdjustmentType
instance ToHeader     ScalingAdjustmentType

instance ToJSON ScalingAdjustmentType where
    toJSON = toJSONText

instance FromJSON ScalingAdjustmentType where
    parseJSON = parseJSONText "ScalingAdjustmentType"
