{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.OnDemandAllocationStrategy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.OnDemandAllocationStrategy (
  OnDemandAllocationStrategy (
    ..
    , ODASLowestPrice
    , ODASPrioritized
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data OnDemandAllocationStrategy = OnDemandAllocationStrategy' (CI
                                                                 Text)
                                    deriving (Eq, Ord, Read, Show, Data,
                                              Typeable, Generic)

pattern ODASLowestPrice :: OnDemandAllocationStrategy
pattern ODASLowestPrice = OnDemandAllocationStrategy' "lowestPrice"

pattern ODASPrioritized :: OnDemandAllocationStrategy
pattern ODASPrioritized = OnDemandAllocationStrategy' "prioritized"

{-# COMPLETE
  ODASLowestPrice,
  ODASPrioritized,
  OnDemandAllocationStrategy' #-}

instance FromText OnDemandAllocationStrategy where
    parser = (OnDemandAllocationStrategy' . mk) <$> takeText

instance ToText OnDemandAllocationStrategy where
    toText (OnDemandAllocationStrategy' ci) = original ci

-- | Represents an enum of /known/ $OnDemandAllocationStrategy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OnDemandAllocationStrategy where
    toEnum i = case i of
        0 -> ODASLowestPrice
        1 -> ODASPrioritized
        _ -> (error . showText) $ "Unknown index for OnDemandAllocationStrategy: " <> toText i
    fromEnum x = case x of
        ODASLowestPrice -> 0
        ODASPrioritized -> 1
        OnDemandAllocationStrategy' name -> (error . showText) $ "Unknown OnDemandAllocationStrategy: " <> original name

-- | Represents the bounds of /known/ $OnDemandAllocationStrategy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OnDemandAllocationStrategy where
    minBound = ODASLowestPrice
    maxBound = ODASPrioritized

instance Hashable     OnDemandAllocationStrategy
instance NFData       OnDemandAllocationStrategy
instance ToByteString OnDemandAllocationStrategy
instance ToQuery      OnDemandAllocationStrategy
instance ToHeader     OnDemandAllocationStrategy

instance FromXML OnDemandAllocationStrategy where
    parseXML = parseXMLText "OnDemandAllocationStrategy"
