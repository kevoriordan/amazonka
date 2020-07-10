{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.PlacementStrategyType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.PlacementStrategyType (
  PlacementStrategyType (
    ..
    , Binpack
    , Random
    , Spread
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PlacementStrategyType = PlacementStrategyType' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern Binpack :: PlacementStrategyType
pattern Binpack = PlacementStrategyType' "binpack"

pattern Random :: PlacementStrategyType
pattern Random = PlacementStrategyType' "random"

pattern Spread :: PlacementStrategyType
pattern Spread = PlacementStrategyType' "spread"

{-# COMPLETE
  Binpack,
  Random,
  Spread,
  PlacementStrategyType' #-}

instance FromText PlacementStrategyType where
    parser = (PlacementStrategyType' . mk) <$> takeText

instance ToText PlacementStrategyType where
    toText (PlacementStrategyType' ci) = original ci

-- | Represents an enum of /known/ $PlacementStrategyType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PlacementStrategyType where
    toEnum i = case i of
        0 -> Binpack
        1 -> Random
        2 -> Spread
        _ -> (error . showText) $ "Unknown index for PlacementStrategyType: " <> toText i
    fromEnum x = case x of
        Binpack -> 0
        Random -> 1
        Spread -> 2
        PlacementStrategyType' name -> (error . showText) $ "Unknown PlacementStrategyType: " <> original name

-- | Represents the bounds of /known/ $PlacementStrategyType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PlacementStrategyType where
    minBound = Binpack
    maxBound = Spread

instance Hashable     PlacementStrategyType
instance NFData       PlacementStrategyType
instance ToByteString PlacementStrategyType
instance ToQuery      PlacementStrategyType
instance ToHeader     PlacementStrategyType

instance ToJSON PlacementStrategyType where
    toJSON = toJSONText

instance FromJSON PlacementStrategyType where
    parseJSON = parseJSONText "PlacementStrategyType"
