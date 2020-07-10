{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetOnDemandAllocationStrategy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.FleetOnDemandAllocationStrategy (
  FleetOnDemandAllocationStrategy (
    ..
    , FODASLowestPrice
    , FODASPrioritized
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data FleetOnDemandAllocationStrategy = FleetOnDemandAllocationStrategy' (CI
                                                                           Text)
                                         deriving (Eq, Ord, Read, Show, Data,
                                                   Typeable, Generic)

pattern FODASLowestPrice :: FleetOnDemandAllocationStrategy
pattern FODASLowestPrice = FleetOnDemandAllocationStrategy' "lowest-price"

pattern FODASPrioritized :: FleetOnDemandAllocationStrategy
pattern FODASPrioritized = FleetOnDemandAllocationStrategy' "prioritized"

{-# COMPLETE
  FODASLowestPrice,
  FODASPrioritized,
  FleetOnDemandAllocationStrategy' #-}

instance FromText FleetOnDemandAllocationStrategy where
    parser = (FleetOnDemandAllocationStrategy' . mk) <$> takeText

instance ToText FleetOnDemandAllocationStrategy where
    toText (FleetOnDemandAllocationStrategy' ci) = original ci

-- | Represents an enum of /known/ $FleetOnDemandAllocationStrategy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FleetOnDemandAllocationStrategy where
    toEnum i = case i of
        0 -> FODASLowestPrice
        1 -> FODASPrioritized
        _ -> (error . showText) $ "Unknown index for FleetOnDemandAllocationStrategy: " <> toText i
    fromEnum x = case x of
        FODASLowestPrice -> 0
        FODASPrioritized -> 1
        FleetOnDemandAllocationStrategy' name -> (error . showText) $ "Unknown FleetOnDemandAllocationStrategy: " <> original name

-- | Represents the bounds of /known/ $FleetOnDemandAllocationStrategy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FleetOnDemandAllocationStrategy where
    minBound = FODASLowestPrice
    maxBound = FODASPrioritized

instance Hashable     FleetOnDemandAllocationStrategy
instance NFData       FleetOnDemandAllocationStrategy
instance ToByteString FleetOnDemandAllocationStrategy
instance ToQuery      FleetOnDemandAllocationStrategy
instance ToHeader     FleetOnDemandAllocationStrategy

instance FromXML FleetOnDemandAllocationStrategy where
    parseXML = parseXMLText "FleetOnDemandAllocationStrategy"
