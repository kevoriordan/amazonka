{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.RoutingStrategyType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.RoutingStrategyType (
  RoutingStrategyType (
    ..
    , Simple
    , Terminal
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RoutingStrategyType = RoutingStrategyType' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern Simple :: RoutingStrategyType
pattern Simple = RoutingStrategyType' "SIMPLE"

pattern Terminal :: RoutingStrategyType
pattern Terminal = RoutingStrategyType' "TERMINAL"

{-# COMPLETE
  Simple,
  Terminal,
  RoutingStrategyType' #-}

instance FromText RoutingStrategyType where
    parser = (RoutingStrategyType' . mk) <$> takeText

instance ToText RoutingStrategyType where
    toText (RoutingStrategyType' ci) = original ci

-- | Represents an enum of /known/ $RoutingStrategyType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RoutingStrategyType where
    toEnum i = case i of
        0 -> Simple
        1 -> Terminal
        _ -> (error . showText) $ "Unknown index for RoutingStrategyType: " <> toText i
    fromEnum x = case x of
        Simple -> 0
        Terminal -> 1
        RoutingStrategyType' name -> (error . showText) $ "Unknown RoutingStrategyType: " <> original name

-- | Represents the bounds of /known/ $RoutingStrategyType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RoutingStrategyType where
    minBound = Simple
    maxBound = Terminal

instance Hashable     RoutingStrategyType
instance NFData       RoutingStrategyType
instance ToByteString RoutingStrategyType
instance ToQuery      RoutingStrategyType
instance ToHeader     RoutingStrategyType

instance ToJSON RoutingStrategyType where
    toJSON = toJSONText

instance FromJSON RoutingStrategyType where
    parseJSON = parseJSONText "RoutingStrategyType"
