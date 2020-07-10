{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.UsageLimitFeatureType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.UsageLimitFeatureType (
  UsageLimitFeatureType (
    ..
    , ConcurrencyScaling
    , Spectrum
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

data UsageLimitFeatureType = UsageLimitFeatureType' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern ConcurrencyScaling :: UsageLimitFeatureType
pattern ConcurrencyScaling = UsageLimitFeatureType' "concurrency-scaling"

pattern Spectrum :: UsageLimitFeatureType
pattern Spectrum = UsageLimitFeatureType' "spectrum"

{-# COMPLETE
  ConcurrencyScaling,
  Spectrum,
  UsageLimitFeatureType' #-}

instance FromText UsageLimitFeatureType where
    parser = (UsageLimitFeatureType' . mk) <$> takeText

instance ToText UsageLimitFeatureType where
    toText (UsageLimitFeatureType' ci) = original ci

-- | Represents an enum of /known/ $UsageLimitFeatureType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum UsageLimitFeatureType where
    toEnum i = case i of
        0 -> ConcurrencyScaling
        1 -> Spectrum
        _ -> (error . showText) $ "Unknown index for UsageLimitFeatureType: " <> toText i
    fromEnum x = case x of
        ConcurrencyScaling -> 0
        Spectrum -> 1
        UsageLimitFeatureType' name -> (error . showText) $ "Unknown UsageLimitFeatureType: " <> original name

-- | Represents the bounds of /known/ $UsageLimitFeatureType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded UsageLimitFeatureType where
    minBound = ConcurrencyScaling
    maxBound = Spectrum

instance Hashable     UsageLimitFeatureType
instance NFData       UsageLimitFeatureType
instance ToByteString UsageLimitFeatureType
instance ToQuery      UsageLimitFeatureType
instance ToHeader     UsageLimitFeatureType

instance FromXML UsageLimitFeatureType where
    parseXML = parseXMLText "UsageLimitFeatureType"
