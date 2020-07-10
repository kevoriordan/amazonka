{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.CRAllocationStrategy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Batch.Types.CRAllocationStrategy (
  CRAllocationStrategy (
    ..
    , BestFit
    , BestFitProgressive
    , SpotCapacityOptimized
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CRAllocationStrategy = CRAllocationStrategy' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern BestFit :: CRAllocationStrategy
pattern BestFit = CRAllocationStrategy' "BEST_FIT"

pattern BestFitProgressive :: CRAllocationStrategy
pattern BestFitProgressive = CRAllocationStrategy' "BEST_FIT_PROGRESSIVE"

pattern SpotCapacityOptimized :: CRAllocationStrategy
pattern SpotCapacityOptimized = CRAllocationStrategy' "SPOT_CAPACITY_OPTIMIZED"

{-# COMPLETE
  BestFit,
  BestFitProgressive,
  SpotCapacityOptimized,
  CRAllocationStrategy' #-}

instance FromText CRAllocationStrategy where
    parser = (CRAllocationStrategy' . mk) <$> takeText

instance ToText CRAllocationStrategy where
    toText (CRAllocationStrategy' ci) = original ci

-- | Represents an enum of /known/ $CRAllocationStrategy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CRAllocationStrategy where
    toEnum i = case i of
        0 -> BestFit
        1 -> BestFitProgressive
        2 -> SpotCapacityOptimized
        _ -> (error . showText) $ "Unknown index for CRAllocationStrategy: " <> toText i
    fromEnum x = case x of
        BestFit -> 0
        BestFitProgressive -> 1
        SpotCapacityOptimized -> 2
        CRAllocationStrategy' name -> (error . showText) $ "Unknown CRAllocationStrategy: " <> original name

-- | Represents the bounds of /known/ $CRAllocationStrategy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CRAllocationStrategy where
    minBound = BestFit
    maxBound = SpotCapacityOptimized

instance Hashable     CRAllocationStrategy
instance NFData       CRAllocationStrategy
instance ToByteString CRAllocationStrategy
instance ToQuery      CRAllocationStrategy
instance ToHeader     CRAllocationStrategy

instance ToJSON CRAllocationStrategy where
    toJSON = toJSONText

instance FromJSON CRAllocationStrategy where
    parseJSON = parseJSONText "CRAllocationStrategy"
