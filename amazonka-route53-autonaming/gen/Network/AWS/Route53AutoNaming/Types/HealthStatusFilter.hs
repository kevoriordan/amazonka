{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.HealthStatusFilter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53AutoNaming.Types.HealthStatusFilter (
  HealthStatusFilter (
    ..
    , HSFAll
    , HSFHealthy
    , HSFUnhealthy
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data HealthStatusFilter = HealthStatusFilter' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern HSFAll :: HealthStatusFilter
pattern HSFAll = HealthStatusFilter' "ALL"

pattern HSFHealthy :: HealthStatusFilter
pattern HSFHealthy = HealthStatusFilter' "HEALTHY"

pattern HSFUnhealthy :: HealthStatusFilter
pattern HSFUnhealthy = HealthStatusFilter' "UNHEALTHY"

{-# COMPLETE
  HSFAll,
  HSFHealthy,
  HSFUnhealthy,
  HealthStatusFilter' #-}

instance FromText HealthStatusFilter where
    parser = (HealthStatusFilter' . mk) <$> takeText

instance ToText HealthStatusFilter where
    toText (HealthStatusFilter' ci) = original ci

-- | Represents an enum of /known/ $HealthStatusFilter.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HealthStatusFilter where
    toEnum i = case i of
        0 -> HSFAll
        1 -> HSFHealthy
        2 -> HSFUnhealthy
        _ -> (error . showText) $ "Unknown index for HealthStatusFilter: " <> toText i
    fromEnum x = case x of
        HSFAll -> 0
        HSFHealthy -> 1
        HSFUnhealthy -> 2
        HealthStatusFilter' name -> (error . showText) $ "Unknown HealthStatusFilter: " <> original name

-- | Represents the bounds of /known/ $HealthStatusFilter.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HealthStatusFilter where
    minBound = HSFAll
    maxBound = HSFUnhealthy

instance Hashable     HealthStatusFilter
instance NFData       HealthStatusFilter
instance ToByteString HealthStatusFilter
instance ToQuery      HealthStatusFilter
instance ToHeader     HealthStatusFilter

instance ToJSON HealthStatusFilter where
    toJSON = toJSONText
