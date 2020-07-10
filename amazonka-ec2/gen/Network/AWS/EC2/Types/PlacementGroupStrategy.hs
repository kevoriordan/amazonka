{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PlacementGroupStrategy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.PlacementGroupStrategy (
  PlacementGroupStrategy (
    ..
    , PGSCluster
    , PGSPartition
    , PGSSpread
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data PlacementGroupStrategy = PlacementGroupStrategy' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern PGSCluster :: PlacementGroupStrategy
pattern PGSCluster = PlacementGroupStrategy' "cluster"

pattern PGSPartition :: PlacementGroupStrategy
pattern PGSPartition = PlacementGroupStrategy' "partition"

pattern PGSSpread :: PlacementGroupStrategy
pattern PGSSpread = PlacementGroupStrategy' "spread"

{-# COMPLETE
  PGSCluster,
  PGSPartition,
  PGSSpread,
  PlacementGroupStrategy' #-}

instance FromText PlacementGroupStrategy where
    parser = (PlacementGroupStrategy' . mk) <$> takeText

instance ToText PlacementGroupStrategy where
    toText (PlacementGroupStrategy' ci) = original ci

-- | Represents an enum of /known/ $PlacementGroupStrategy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PlacementGroupStrategy where
    toEnum i = case i of
        0 -> PGSCluster
        1 -> PGSPartition
        2 -> PGSSpread
        _ -> (error . showText) $ "Unknown index for PlacementGroupStrategy: " <> toText i
    fromEnum x = case x of
        PGSCluster -> 0
        PGSPartition -> 1
        PGSSpread -> 2
        PlacementGroupStrategy' name -> (error . showText) $ "Unknown PlacementGroupStrategy: " <> original name

-- | Represents the bounds of /known/ $PlacementGroupStrategy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PlacementGroupStrategy where
    minBound = PGSCluster
    maxBound = PGSSpread

instance Hashable     PlacementGroupStrategy
instance NFData       PlacementGroupStrategy
instance ToByteString PlacementGroupStrategy
instance ToQuery      PlacementGroupStrategy
instance ToHeader     PlacementGroupStrategy

instance FromXML PlacementGroupStrategy where
    parseXML = parseXMLText "PlacementGroupStrategy"
