{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AllocationStrategy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.AllocationStrategy (
  AllocationStrategy (
    ..
    , ASCapacityOptimized
    , ASDiversified
    , ASLowestPrice
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data AllocationStrategy = AllocationStrategy' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern ASCapacityOptimized :: AllocationStrategy
pattern ASCapacityOptimized = AllocationStrategy' "capacityOptimized"

pattern ASDiversified :: AllocationStrategy
pattern ASDiversified = AllocationStrategy' "diversified"

pattern ASLowestPrice :: AllocationStrategy
pattern ASLowestPrice = AllocationStrategy' "lowestPrice"

{-# COMPLETE
  ASCapacityOptimized,
  ASDiversified,
  ASLowestPrice,
  AllocationStrategy' #-}

instance FromText AllocationStrategy where
    parser = (AllocationStrategy' . mk) <$> takeText

instance ToText AllocationStrategy where
    toText (AllocationStrategy' ci) = original ci

-- | Represents an enum of /known/ $AllocationStrategy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AllocationStrategy where
    toEnum i = case i of
        0 -> ASCapacityOptimized
        1 -> ASDiversified
        2 -> ASLowestPrice
        _ -> (error . showText) $ "Unknown index for AllocationStrategy: " <> toText i
    fromEnum x = case x of
        ASCapacityOptimized -> 0
        ASDiversified -> 1
        ASLowestPrice -> 2
        AllocationStrategy' name -> (error . showText) $ "Unknown AllocationStrategy: " <> original name

-- | Represents the bounds of /known/ $AllocationStrategy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AllocationStrategy where
    minBound = ASCapacityOptimized
    maxBound = ASLowestPrice

instance Hashable     AllocationStrategy
instance NFData       AllocationStrategy
instance ToByteString AllocationStrategy
instance ToQuery      AllocationStrategy
instance ToHeader     AllocationStrategy

instance FromXML AllocationStrategy where
    parseXML = parseXMLText "AllocationStrategy"
