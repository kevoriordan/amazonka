{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PlacementStrategy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.PlacementStrategy (
  PlacementStrategy (
    ..
    , Cluster
    , Partition
    , Spread
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data PlacementStrategy = PlacementStrategy' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern Cluster :: PlacementStrategy
pattern Cluster = PlacementStrategy' "cluster"

pattern Partition :: PlacementStrategy
pattern Partition = PlacementStrategy' "partition"

pattern Spread :: PlacementStrategy
pattern Spread = PlacementStrategy' "spread"

{-# COMPLETE
  Cluster,
  Partition,
  Spread,
  PlacementStrategy' #-}

instance FromText PlacementStrategy where
    parser = (PlacementStrategy' . mk) <$> takeText

instance ToText PlacementStrategy where
    toText (PlacementStrategy' ci) = original ci

-- | Represents an enum of /known/ $PlacementStrategy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PlacementStrategy where
    toEnum i = case i of
        0 -> Cluster
        1 -> Partition
        2 -> Spread
        _ -> (error . showText) $ "Unknown index for PlacementStrategy: " <> toText i
    fromEnum x = case x of
        Cluster -> 0
        Partition -> 1
        Spread -> 2
        PlacementStrategy' name -> (error . showText) $ "Unknown PlacementStrategy: " <> original name

-- | Represents the bounds of /known/ $PlacementStrategy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PlacementStrategy where
    minBound = Cluster
    maxBound = Spread

instance Hashable     PlacementStrategy
instance NFData       PlacementStrategy
instance ToByteString PlacementStrategy
instance ToQuery      PlacementStrategy
instance ToHeader     PlacementStrategy

instance FromXML PlacementStrategy where
    parseXML = parseXMLText "PlacementStrategy"
