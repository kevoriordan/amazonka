{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.ThresholdType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Budgets.Types.ThresholdType (
  ThresholdType (
    ..
    , AbsoluteValue
    , Percentage
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The type of threshold for a notification. It can be PERCENTAGE or ABSOLUTE_VALUE.
--
--
data ThresholdType = ThresholdType' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern AbsoluteValue :: ThresholdType
pattern AbsoluteValue = ThresholdType' "ABSOLUTE_VALUE"

pattern Percentage :: ThresholdType
pattern Percentage = ThresholdType' "PERCENTAGE"

{-# COMPLETE
  AbsoluteValue,
  Percentage,
  ThresholdType' #-}

instance FromText ThresholdType where
    parser = (ThresholdType' . mk) <$> takeText

instance ToText ThresholdType where
    toText (ThresholdType' ci) = original ci

-- | Represents an enum of /known/ $ThresholdType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ThresholdType where
    toEnum i = case i of
        0 -> AbsoluteValue
        1 -> Percentage
        _ -> (error . showText) $ "Unknown index for ThresholdType: " <> toText i
    fromEnum x = case x of
        AbsoluteValue -> 0
        Percentage -> 1
        ThresholdType' name -> (error . showText) $ "Unknown ThresholdType: " <> original name

-- | Represents the bounds of /known/ $ThresholdType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ThresholdType where
    minBound = AbsoluteValue
    maxBound = Percentage

instance Hashable     ThresholdType
instance NFData       ThresholdType
instance ToByteString ThresholdType
instance ToQuery      ThresholdType
instance ToHeader     ThresholdType

instance ToJSON ThresholdType where
    toJSON = toJSONText

instance FromJSON ThresholdType where
    parseJSON = parseJSONText "ThresholdType"
