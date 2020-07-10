{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TrafficRoutingType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.TrafficRoutingType (
  TrafficRoutingType (
    ..
    , AllAtOnce
    , TimeBasedCanary
    , TimeBasedLinear
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TrafficRoutingType = TrafficRoutingType' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern AllAtOnce :: TrafficRoutingType
pattern AllAtOnce = TrafficRoutingType' "AllAtOnce"

pattern TimeBasedCanary :: TrafficRoutingType
pattern TimeBasedCanary = TrafficRoutingType' "TimeBasedCanary"

pattern TimeBasedLinear :: TrafficRoutingType
pattern TimeBasedLinear = TrafficRoutingType' "TimeBasedLinear"

{-# COMPLETE
  AllAtOnce,
  TimeBasedCanary,
  TimeBasedLinear,
  TrafficRoutingType' #-}

instance FromText TrafficRoutingType where
    parser = (TrafficRoutingType' . mk) <$> takeText

instance ToText TrafficRoutingType where
    toText (TrafficRoutingType' ci) = original ci

-- | Represents an enum of /known/ $TrafficRoutingType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TrafficRoutingType where
    toEnum i = case i of
        0 -> AllAtOnce
        1 -> TimeBasedCanary
        2 -> TimeBasedLinear
        _ -> (error . showText) $ "Unknown index for TrafficRoutingType: " <> toText i
    fromEnum x = case x of
        AllAtOnce -> 0
        TimeBasedCanary -> 1
        TimeBasedLinear -> 2
        TrafficRoutingType' name -> (error . showText) $ "Unknown TrafficRoutingType: " <> original name

-- | Represents the bounds of /known/ $TrafficRoutingType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TrafficRoutingType where
    minBound = AllAtOnce
    maxBound = TimeBasedLinear

instance Hashable     TrafficRoutingType
instance NFData       TrafficRoutingType
instance ToByteString TrafficRoutingType
instance ToQuery      TrafficRoutingType
instance ToHeader     TrafficRoutingType

instance ToJSON TrafficRoutingType where
    toJSON = toJSONText

instance FromJSON TrafficRoutingType where
    parseJSON = parseJSONText "TrafficRoutingType"
