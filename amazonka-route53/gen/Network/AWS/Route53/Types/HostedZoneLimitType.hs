{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.HostedZoneLimitType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types.HostedZoneLimitType (
  HostedZoneLimitType (
    ..
    , MaxRrsetsByZone
    , MaxVPCsAssociatedByZone
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.Route53.Internal

data HostedZoneLimitType = HostedZoneLimitType' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern MaxRrsetsByZone :: HostedZoneLimitType
pattern MaxRrsetsByZone = HostedZoneLimitType' "MAX_RRSETS_BY_ZONE"

pattern MaxVPCsAssociatedByZone :: HostedZoneLimitType
pattern MaxVPCsAssociatedByZone = HostedZoneLimitType' "MAX_VPCS_ASSOCIATED_BY_ZONE"

{-# COMPLETE
  MaxRrsetsByZone,
  MaxVPCsAssociatedByZone,
  HostedZoneLimitType' #-}

instance FromText HostedZoneLimitType where
    parser = (HostedZoneLimitType' . mk) <$> takeText

instance ToText HostedZoneLimitType where
    toText (HostedZoneLimitType' ci) = original ci

-- | Represents an enum of /known/ $HostedZoneLimitType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HostedZoneLimitType where
    toEnum i = case i of
        0 -> MaxRrsetsByZone
        1 -> MaxVPCsAssociatedByZone
        _ -> (error . showText) $ "Unknown index for HostedZoneLimitType: " <> toText i
    fromEnum x = case x of
        MaxRrsetsByZone -> 0
        MaxVPCsAssociatedByZone -> 1
        HostedZoneLimitType' name -> (error . showText) $ "Unknown HostedZoneLimitType: " <> original name

-- | Represents the bounds of /known/ $HostedZoneLimitType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HostedZoneLimitType where
    minBound = MaxRrsetsByZone
    maxBound = MaxVPCsAssociatedByZone

instance Hashable     HostedZoneLimitType
instance NFData       HostedZoneLimitType
instance ToByteString HostedZoneLimitType
instance ToQuery      HostedZoneLimitType
instance ToHeader     HostedZoneLimitType

instance FromXML HostedZoneLimitType where
    parseXML = parseXMLText "HostedZoneLimitType"

instance ToXML HostedZoneLimitType where
    toXML = toXMLText
