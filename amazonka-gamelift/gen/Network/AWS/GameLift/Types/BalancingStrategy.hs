{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.BalancingStrategy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.BalancingStrategy (
  BalancingStrategy (
    ..
    , SpotOnly
    , SpotPreferred
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data BalancingStrategy = BalancingStrategy' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern SpotOnly :: BalancingStrategy
pattern SpotOnly = BalancingStrategy' "SPOT_ONLY"

pattern SpotPreferred :: BalancingStrategy
pattern SpotPreferred = BalancingStrategy' "SPOT_PREFERRED"

{-# COMPLETE
  SpotOnly,
  SpotPreferred,
  BalancingStrategy' #-}

instance FromText BalancingStrategy where
    parser = (BalancingStrategy' . mk) <$> takeText

instance ToText BalancingStrategy where
    toText (BalancingStrategy' ci) = original ci

-- | Represents an enum of /known/ $BalancingStrategy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BalancingStrategy where
    toEnum i = case i of
        0 -> SpotOnly
        1 -> SpotPreferred
        _ -> (error . showText) $ "Unknown index for BalancingStrategy: " <> toText i
    fromEnum x = case x of
        SpotOnly -> 0
        SpotPreferred -> 1
        BalancingStrategy' name -> (error . showText) $ "Unknown BalancingStrategy: " <> original name

-- | Represents the bounds of /known/ $BalancingStrategy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BalancingStrategy where
    minBound = SpotOnly
    maxBound = SpotPreferred

instance Hashable     BalancingStrategy
instance NFData       BalancingStrategy
instance ToByteString BalancingStrategy
instance ToQuery      BalancingStrategy
instance ToHeader     BalancingStrategy

instance ToJSON BalancingStrategy where
    toJSON = toJSONText

instance FromJSON BalancingStrategy where
    parseJSON = parseJSONText "BalancingStrategy"
