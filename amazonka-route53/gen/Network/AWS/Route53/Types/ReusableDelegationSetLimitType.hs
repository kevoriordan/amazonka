{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.ReusableDelegationSetLimitType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types.ReusableDelegationSetLimitType (
  ReusableDelegationSetLimitType (
    ..
    , MaxZonesByReusableDelegationSet
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.Route53.Internal

data ReusableDelegationSetLimitType = ReusableDelegationSetLimitType' (CI
                                                                         Text)
                                        deriving (Eq, Ord, Read, Show, Data,
                                                  Typeable, Generic)

pattern MaxZonesByReusableDelegationSet :: ReusableDelegationSetLimitType
pattern MaxZonesByReusableDelegationSet = ReusableDelegationSetLimitType' "MAX_ZONES_BY_REUSABLE_DELEGATION_SET"

{-# COMPLETE
  MaxZonesByReusableDelegationSet,
  ReusableDelegationSetLimitType' #-}

instance FromText ReusableDelegationSetLimitType where
    parser = (ReusableDelegationSetLimitType' . mk) <$> takeText

instance ToText ReusableDelegationSetLimitType where
    toText (ReusableDelegationSetLimitType' ci) = original ci

-- | Represents an enum of /known/ $ReusableDelegationSetLimitType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReusableDelegationSetLimitType where
    toEnum i = case i of
        0 -> MaxZonesByReusableDelegationSet
        _ -> (error . showText) $ "Unknown index for ReusableDelegationSetLimitType: " <> toText i
    fromEnum x = case x of
        MaxZonesByReusableDelegationSet -> 0
        ReusableDelegationSetLimitType' name -> (error . showText) $ "Unknown ReusableDelegationSetLimitType: " <> original name

-- | Represents the bounds of /known/ $ReusableDelegationSetLimitType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReusableDelegationSetLimitType where
    minBound = MaxZonesByReusableDelegationSet
    maxBound = MaxZonesByReusableDelegationSet

instance Hashable     ReusableDelegationSetLimitType
instance NFData       ReusableDelegationSetLimitType
instance ToByteString ReusableDelegationSetLimitType
instance ToQuery      ReusableDelegationSetLimitType
instance ToHeader     ReusableDelegationSetLimitType

instance FromXML ReusableDelegationSetLimitType where
    parseXML = parseXMLText "ReusableDelegationSetLimitType"

instance ToXML ReusableDelegationSetLimitType where
    toXML = toXMLText
