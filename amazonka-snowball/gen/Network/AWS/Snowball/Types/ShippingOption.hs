{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.ShippingOption
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Snowball.Types.ShippingOption (
  ShippingOption (
    ..
    , SOExpress
    , SONextDay
    , SOSecondDay
    , SOStandard
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ShippingOption = ShippingOption' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern SOExpress :: ShippingOption
pattern SOExpress = ShippingOption' "EXPRESS"

pattern SONextDay :: ShippingOption
pattern SONextDay = ShippingOption' "NEXT_DAY"

pattern SOSecondDay :: ShippingOption
pattern SOSecondDay = ShippingOption' "SECOND_DAY"

pattern SOStandard :: ShippingOption
pattern SOStandard = ShippingOption' "STANDARD"

{-# COMPLETE
  SOExpress,
  SONextDay,
  SOSecondDay,
  SOStandard,
  ShippingOption' #-}

instance FromText ShippingOption where
    parser = (ShippingOption' . mk) <$> takeText

instance ToText ShippingOption where
    toText (ShippingOption' ci) = original ci

-- | Represents an enum of /known/ $ShippingOption.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ShippingOption where
    toEnum i = case i of
        0 -> SOExpress
        1 -> SONextDay
        2 -> SOSecondDay
        3 -> SOStandard
        _ -> (error . showText) $ "Unknown index for ShippingOption: " <> toText i
    fromEnum x = case x of
        SOExpress -> 0
        SONextDay -> 1
        SOSecondDay -> 2
        SOStandard -> 3
        ShippingOption' name -> (error . showText) $ "Unknown ShippingOption: " <> original name

-- | Represents the bounds of /known/ $ShippingOption.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ShippingOption where
    minBound = SOExpress
    maxBound = SOStandard

instance Hashable     ShippingOption
instance NFData       ShippingOption
instance ToByteString ShippingOption
instance ToQuery      ShippingOption
instance ToHeader     ShippingOption

instance ToJSON ShippingOption where
    toJSON = toJSONText

instance FromJSON ShippingOption where
    parseJSON = parseJSONText "ShippingOption"
