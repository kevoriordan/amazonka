{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotAllocationStrategy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.SpotAllocationStrategy (
  SpotAllocationStrategy (
    ..
    , CapacityOptimized
    , Diversified
    , LowestPrice
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data SpotAllocationStrategy = SpotAllocationStrategy' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern CapacityOptimized :: SpotAllocationStrategy
pattern CapacityOptimized = SpotAllocationStrategy' "capacity-optimized"

pattern Diversified :: SpotAllocationStrategy
pattern Diversified = SpotAllocationStrategy' "diversified"

pattern LowestPrice :: SpotAllocationStrategy
pattern LowestPrice = SpotAllocationStrategy' "lowest-price"

{-# COMPLETE
  CapacityOptimized,
  Diversified,
  LowestPrice,
  SpotAllocationStrategy' #-}

instance FromText SpotAllocationStrategy where
    parser = (SpotAllocationStrategy' . mk) <$> takeText

instance ToText SpotAllocationStrategy where
    toText (SpotAllocationStrategy' ci) = original ci

-- | Represents an enum of /known/ $SpotAllocationStrategy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SpotAllocationStrategy where
    toEnum i = case i of
        0 -> CapacityOptimized
        1 -> Diversified
        2 -> LowestPrice
        _ -> (error . showText) $ "Unknown index for SpotAllocationStrategy: " <> toText i
    fromEnum x = case x of
        CapacityOptimized -> 0
        Diversified -> 1
        LowestPrice -> 2
        SpotAllocationStrategy' name -> (error . showText) $ "Unknown SpotAllocationStrategy: " <> original name

-- | Represents the bounds of /known/ $SpotAllocationStrategy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SpotAllocationStrategy where
    minBound = CapacityOptimized
    maxBound = LowestPrice

instance Hashable     SpotAllocationStrategy
instance NFData       SpotAllocationStrategy
instance ToByteString SpotAllocationStrategy
instance ToQuery      SpotAllocationStrategy
instance ToHeader     SpotAllocationStrategy

instance FromXML SpotAllocationStrategy where
    parseXML = parseXMLText "SpotAllocationStrategy"
