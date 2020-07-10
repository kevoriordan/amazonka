{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.TemperatureUnit
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.TemperatureUnit (
  TemperatureUnit (
    ..
    , Celsius
    , Fahrenheit
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TemperatureUnit = TemperatureUnit' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Celsius :: TemperatureUnit
pattern Celsius = TemperatureUnit' "CELSIUS"

pattern Fahrenheit :: TemperatureUnit
pattern Fahrenheit = TemperatureUnit' "FAHRENHEIT"

{-# COMPLETE
  Celsius,
  Fahrenheit,
  TemperatureUnit' #-}

instance FromText TemperatureUnit where
    parser = (TemperatureUnit' . mk) <$> takeText

instance ToText TemperatureUnit where
    toText (TemperatureUnit' ci) = original ci

-- | Represents an enum of /known/ $TemperatureUnit.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TemperatureUnit where
    toEnum i = case i of
        0 -> Celsius
        1 -> Fahrenheit
        _ -> (error . showText) $ "Unknown index for TemperatureUnit: " <> toText i
    fromEnum x = case x of
        Celsius -> 0
        Fahrenheit -> 1
        TemperatureUnit' name -> (error . showText) $ "Unknown TemperatureUnit: " <> original name

-- | Represents the bounds of /known/ $TemperatureUnit.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TemperatureUnit where
    minBound = Celsius
    maxBound = Fahrenheit

instance Hashable     TemperatureUnit
instance NFData       TemperatureUnit
instance ToByteString TemperatureUnit
instance ToQuery      TemperatureUnit
instance ToHeader     TemperatureUnit

instance ToJSON TemperatureUnit where
    toJSON = toJSONText

instance FromJSON TemperatureUnit where
    parseJSON = parseJSONText "TemperatureUnit"
