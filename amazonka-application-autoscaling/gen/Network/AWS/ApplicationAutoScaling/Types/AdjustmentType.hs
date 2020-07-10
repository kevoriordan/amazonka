{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.AdjustmentType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApplicationAutoScaling.Types.AdjustmentType (
  AdjustmentType (
    ..
    , ChangeInCapacity
    , ExactCapacity
    , PercentChangeInCapacity
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AdjustmentType = AdjustmentType' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern ChangeInCapacity :: AdjustmentType
pattern ChangeInCapacity = AdjustmentType' "ChangeInCapacity"

pattern ExactCapacity :: AdjustmentType
pattern ExactCapacity = AdjustmentType' "ExactCapacity"

pattern PercentChangeInCapacity :: AdjustmentType
pattern PercentChangeInCapacity = AdjustmentType' "PercentChangeInCapacity"

{-# COMPLETE
  ChangeInCapacity,
  ExactCapacity,
  PercentChangeInCapacity,
  AdjustmentType' #-}

instance FromText AdjustmentType where
    parser = (AdjustmentType' . mk) <$> takeText

instance ToText AdjustmentType where
    toText (AdjustmentType' ci) = original ci

-- | Represents an enum of /known/ $AdjustmentType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AdjustmentType where
    toEnum i = case i of
        0 -> ChangeInCapacity
        1 -> ExactCapacity
        2 -> PercentChangeInCapacity
        _ -> (error . showText) $ "Unknown index for AdjustmentType: " <> toText i
    fromEnum x = case x of
        ChangeInCapacity -> 0
        ExactCapacity -> 1
        PercentChangeInCapacity -> 2
        AdjustmentType' name -> (error . showText) $ "Unknown AdjustmentType: " <> original name

-- | Represents the bounds of /known/ $AdjustmentType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AdjustmentType where
    minBound = ChangeInCapacity
    maxBound = PercentChangeInCapacity

instance Hashable     AdjustmentType
instance NFData       AdjustmentType
instance ToByteString AdjustmentType
instance ToQuery      AdjustmentType
instance ToHeader     AdjustmentType

instance ToJSON AdjustmentType where
    toJSON = toJSONText

instance FromJSON AdjustmentType where
    parseJSON = parseJSONText "AdjustmentType"
