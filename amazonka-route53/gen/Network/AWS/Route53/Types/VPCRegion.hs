{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.VPCRegion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types.VPCRegion (
  VPCRegion (
    ..
    , AfSouth1
    , ApEast1
    , ApNortheast1
    , ApNortheast2
    , ApNortheast3
    , ApSouth1
    , ApSoutheast1
    , ApSoutheast2
    , CaCentral1
    , CnNorth1
    , EuCentral1
    , EuNorth1
    , EuWest1
    , EuWest2
    , EuWest3
    , MeSouth1
    , SaEast1
    , UsEast1
    , UsEast2
    , UsGovEast1
    , UsGovWest1
    , UsIsoEast1
    , UsIsobEast1
    , UsWest1
    , UsWest2
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.Route53.Internal

data VPCRegion = VPCRegion' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern AfSouth1 :: VPCRegion
pattern AfSouth1 = VPCRegion' "af-south-1"

pattern ApEast1 :: VPCRegion
pattern ApEast1 = VPCRegion' "ap-east-1"

pattern ApNortheast1 :: VPCRegion
pattern ApNortheast1 = VPCRegion' "ap-northeast-1"

pattern ApNortheast2 :: VPCRegion
pattern ApNortheast2 = VPCRegion' "ap-northeast-2"

pattern ApNortheast3 :: VPCRegion
pattern ApNortheast3 = VPCRegion' "ap-northeast-3"

pattern ApSouth1 :: VPCRegion
pattern ApSouth1 = VPCRegion' "ap-south-1"

pattern ApSoutheast1 :: VPCRegion
pattern ApSoutheast1 = VPCRegion' "ap-southeast-1"

pattern ApSoutheast2 :: VPCRegion
pattern ApSoutheast2 = VPCRegion' "ap-southeast-2"

pattern CaCentral1 :: VPCRegion
pattern CaCentral1 = VPCRegion' "ca-central-1"

pattern CnNorth1 :: VPCRegion
pattern CnNorth1 = VPCRegion' "cn-north-1"

pattern EuCentral1 :: VPCRegion
pattern EuCentral1 = VPCRegion' "eu-central-1"

pattern EuNorth1 :: VPCRegion
pattern EuNorth1 = VPCRegion' "eu-north-1"

pattern EuWest1 :: VPCRegion
pattern EuWest1 = VPCRegion' "eu-west-1"

pattern EuWest2 :: VPCRegion
pattern EuWest2 = VPCRegion' "eu-west-2"

pattern EuWest3 :: VPCRegion
pattern EuWest3 = VPCRegion' "eu-west-3"

pattern MeSouth1 :: VPCRegion
pattern MeSouth1 = VPCRegion' "me-south-1"

pattern SaEast1 :: VPCRegion
pattern SaEast1 = VPCRegion' "sa-east-1"

pattern UsEast1 :: VPCRegion
pattern UsEast1 = VPCRegion' "us-east-1"

pattern UsEast2 :: VPCRegion
pattern UsEast2 = VPCRegion' "us-east-2"

pattern UsGovEast1 :: VPCRegion
pattern UsGovEast1 = VPCRegion' "us-gov-east-1"

pattern UsGovWest1 :: VPCRegion
pattern UsGovWest1 = VPCRegion' "us-gov-west-1"

pattern UsIsoEast1 :: VPCRegion
pattern UsIsoEast1 = VPCRegion' "us-iso-east-1"

pattern UsIsobEast1 :: VPCRegion
pattern UsIsobEast1 = VPCRegion' "us-isob-east-1"

pattern UsWest1 :: VPCRegion
pattern UsWest1 = VPCRegion' "us-west-1"

pattern UsWest2 :: VPCRegion
pattern UsWest2 = VPCRegion' "us-west-2"

{-# COMPLETE
  AfSouth1,
  ApEast1,
  ApNortheast1,
  ApNortheast2,
  ApNortheast3,
  ApSouth1,
  ApSoutheast1,
  ApSoutheast2,
  CaCentral1,
  CnNorth1,
  EuCentral1,
  EuNorth1,
  EuWest1,
  EuWest2,
  EuWest3,
  MeSouth1,
  SaEast1,
  UsEast1,
  UsEast2,
  UsGovEast1,
  UsGovWest1,
  UsIsoEast1,
  UsIsobEast1,
  UsWest1,
  UsWest2,
  VPCRegion' #-}

instance FromText VPCRegion where
    parser = (VPCRegion' . mk) <$> takeText

instance ToText VPCRegion where
    toText (VPCRegion' ci) = original ci

-- | Represents an enum of /known/ $VPCRegion.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum VPCRegion where
    toEnum i = case i of
        0 -> AfSouth1
        1 -> ApEast1
        2 -> ApNortheast1
        3 -> ApNortheast2
        4 -> ApNortheast3
        5 -> ApSouth1
        6 -> ApSoutheast1
        7 -> ApSoutheast2
        8 -> CaCentral1
        9 -> CnNorth1
        10 -> EuCentral1
        11 -> EuNorth1
        12 -> EuWest1
        13 -> EuWest2
        14 -> EuWest3
        15 -> MeSouth1
        16 -> SaEast1
        17 -> UsEast1
        18 -> UsEast2
        19 -> UsGovEast1
        20 -> UsGovWest1
        21 -> UsIsoEast1
        22 -> UsIsobEast1
        23 -> UsWest1
        24 -> UsWest2
        _ -> (error . showText) $ "Unknown index for VPCRegion: " <> toText i
    fromEnum x = case x of
        AfSouth1 -> 0
        ApEast1 -> 1
        ApNortheast1 -> 2
        ApNortheast2 -> 3
        ApNortheast3 -> 4
        ApSouth1 -> 5
        ApSoutheast1 -> 6
        ApSoutheast2 -> 7
        CaCentral1 -> 8
        CnNorth1 -> 9
        EuCentral1 -> 10
        EuNorth1 -> 11
        EuWest1 -> 12
        EuWest2 -> 13
        EuWest3 -> 14
        MeSouth1 -> 15
        SaEast1 -> 16
        UsEast1 -> 17
        UsEast2 -> 18
        UsGovEast1 -> 19
        UsGovWest1 -> 20
        UsIsoEast1 -> 21
        UsIsobEast1 -> 22
        UsWest1 -> 23
        UsWest2 -> 24
        VPCRegion' name -> (error . showText) $ "Unknown VPCRegion: " <> original name

-- | Represents the bounds of /known/ $VPCRegion.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded VPCRegion where
    minBound = AfSouth1
    maxBound = UsWest2

instance Hashable     VPCRegion
instance NFData       VPCRegion
instance ToByteString VPCRegion
instance ToQuery      VPCRegion
instance ToHeader     VPCRegion

instance FromXML VPCRegion where
    parseXML = parseXMLText "VPCRegion"

instance ToXML VPCRegion where
    toXML = toXMLText
