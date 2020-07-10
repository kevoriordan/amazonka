{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.CloudWatchRegion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types.CloudWatchRegion (
  CloudWatchRegion (
    ..
    , CWRAfSouth1
    , CWRApEast1
    , CWRApNortheast1
    , CWRApNortheast2
    , CWRApNortheast3
    , CWRApSouth1
    , CWRApSoutheast1
    , CWRApSoutheast2
    , CWRCaCentral1
    , CWRCnNorth1
    , CWRCnNorthwest1
    , CWREuCentral1
    , CWREuNorth1
    , CWREuWest1
    , CWREuWest2
    , CWREuWest3
    , CWRMeSouth1
    , CWRSaEast1
    , CWRUsEast1
    , CWRUsEast2
    , CWRUsGovEast1
    , CWRUsGovWest1
    , CWRUsIsoEast1
    , CWRUsIsobEast1
    , CWRUsWest1
    , CWRUsWest2
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.Route53.Internal

data CloudWatchRegion = CloudWatchRegion' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern CWRAfSouth1 :: CloudWatchRegion
pattern CWRAfSouth1 = CloudWatchRegion' "af-south-1"

pattern CWRApEast1 :: CloudWatchRegion
pattern CWRApEast1 = CloudWatchRegion' "ap-east-1"

pattern CWRApNortheast1 :: CloudWatchRegion
pattern CWRApNortheast1 = CloudWatchRegion' "ap-northeast-1"

pattern CWRApNortheast2 :: CloudWatchRegion
pattern CWRApNortheast2 = CloudWatchRegion' "ap-northeast-2"

pattern CWRApNortheast3 :: CloudWatchRegion
pattern CWRApNortheast3 = CloudWatchRegion' "ap-northeast-3"

pattern CWRApSouth1 :: CloudWatchRegion
pattern CWRApSouth1 = CloudWatchRegion' "ap-south-1"

pattern CWRApSoutheast1 :: CloudWatchRegion
pattern CWRApSoutheast1 = CloudWatchRegion' "ap-southeast-1"

pattern CWRApSoutheast2 :: CloudWatchRegion
pattern CWRApSoutheast2 = CloudWatchRegion' "ap-southeast-2"

pattern CWRCaCentral1 :: CloudWatchRegion
pattern CWRCaCentral1 = CloudWatchRegion' "ca-central-1"

pattern CWRCnNorth1 :: CloudWatchRegion
pattern CWRCnNorth1 = CloudWatchRegion' "cn-north-1"

pattern CWRCnNorthwest1 :: CloudWatchRegion
pattern CWRCnNorthwest1 = CloudWatchRegion' "cn-northwest-1"

pattern CWREuCentral1 :: CloudWatchRegion
pattern CWREuCentral1 = CloudWatchRegion' "eu-central-1"

pattern CWREuNorth1 :: CloudWatchRegion
pattern CWREuNorth1 = CloudWatchRegion' "eu-north-1"

pattern CWREuWest1 :: CloudWatchRegion
pattern CWREuWest1 = CloudWatchRegion' "eu-west-1"

pattern CWREuWest2 :: CloudWatchRegion
pattern CWREuWest2 = CloudWatchRegion' "eu-west-2"

pattern CWREuWest3 :: CloudWatchRegion
pattern CWREuWest3 = CloudWatchRegion' "eu-west-3"

pattern CWRMeSouth1 :: CloudWatchRegion
pattern CWRMeSouth1 = CloudWatchRegion' "me-south-1"

pattern CWRSaEast1 :: CloudWatchRegion
pattern CWRSaEast1 = CloudWatchRegion' "sa-east-1"

pattern CWRUsEast1 :: CloudWatchRegion
pattern CWRUsEast1 = CloudWatchRegion' "us-east-1"

pattern CWRUsEast2 :: CloudWatchRegion
pattern CWRUsEast2 = CloudWatchRegion' "us-east-2"

pattern CWRUsGovEast1 :: CloudWatchRegion
pattern CWRUsGovEast1 = CloudWatchRegion' "us-gov-east-1"

pattern CWRUsGovWest1 :: CloudWatchRegion
pattern CWRUsGovWest1 = CloudWatchRegion' "us-gov-west-1"

pattern CWRUsIsoEast1 :: CloudWatchRegion
pattern CWRUsIsoEast1 = CloudWatchRegion' "us-iso-east-1"

pattern CWRUsIsobEast1 :: CloudWatchRegion
pattern CWRUsIsobEast1 = CloudWatchRegion' "us-isob-east-1"

pattern CWRUsWest1 :: CloudWatchRegion
pattern CWRUsWest1 = CloudWatchRegion' "us-west-1"

pattern CWRUsWest2 :: CloudWatchRegion
pattern CWRUsWest2 = CloudWatchRegion' "us-west-2"

{-# COMPLETE
  CWRAfSouth1,
  CWRApEast1,
  CWRApNortheast1,
  CWRApNortheast2,
  CWRApNortheast3,
  CWRApSouth1,
  CWRApSoutheast1,
  CWRApSoutheast2,
  CWRCaCentral1,
  CWRCnNorth1,
  CWRCnNorthwest1,
  CWREuCentral1,
  CWREuNorth1,
  CWREuWest1,
  CWREuWest2,
  CWREuWest3,
  CWRMeSouth1,
  CWRSaEast1,
  CWRUsEast1,
  CWRUsEast2,
  CWRUsGovEast1,
  CWRUsGovWest1,
  CWRUsIsoEast1,
  CWRUsIsobEast1,
  CWRUsWest1,
  CWRUsWest2,
  CloudWatchRegion' #-}

instance FromText CloudWatchRegion where
    parser = (CloudWatchRegion' . mk) <$> takeText

instance ToText CloudWatchRegion where
    toText (CloudWatchRegion' ci) = original ci

-- | Represents an enum of /known/ $CloudWatchRegion.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CloudWatchRegion where
    toEnum i = case i of
        0 -> CWRAfSouth1
        1 -> CWRApEast1
        2 -> CWRApNortheast1
        3 -> CWRApNortheast2
        4 -> CWRApNortheast3
        5 -> CWRApSouth1
        6 -> CWRApSoutheast1
        7 -> CWRApSoutheast2
        8 -> CWRCaCentral1
        9 -> CWRCnNorth1
        10 -> CWRCnNorthwest1
        11 -> CWREuCentral1
        12 -> CWREuNorth1
        13 -> CWREuWest1
        14 -> CWREuWest2
        15 -> CWREuWest3
        16 -> CWRMeSouth1
        17 -> CWRSaEast1
        18 -> CWRUsEast1
        19 -> CWRUsEast2
        20 -> CWRUsGovEast1
        21 -> CWRUsGovWest1
        22 -> CWRUsIsoEast1
        23 -> CWRUsIsobEast1
        24 -> CWRUsWest1
        25 -> CWRUsWest2
        _ -> (error . showText) $ "Unknown index for CloudWatchRegion: " <> toText i
    fromEnum x = case x of
        CWRAfSouth1 -> 0
        CWRApEast1 -> 1
        CWRApNortheast1 -> 2
        CWRApNortheast2 -> 3
        CWRApNortheast3 -> 4
        CWRApSouth1 -> 5
        CWRApSoutheast1 -> 6
        CWRApSoutheast2 -> 7
        CWRCaCentral1 -> 8
        CWRCnNorth1 -> 9
        CWRCnNorthwest1 -> 10
        CWREuCentral1 -> 11
        CWREuNorth1 -> 12
        CWREuWest1 -> 13
        CWREuWest2 -> 14
        CWREuWest3 -> 15
        CWRMeSouth1 -> 16
        CWRSaEast1 -> 17
        CWRUsEast1 -> 18
        CWRUsEast2 -> 19
        CWRUsGovEast1 -> 20
        CWRUsGovWest1 -> 21
        CWRUsIsoEast1 -> 22
        CWRUsIsobEast1 -> 23
        CWRUsWest1 -> 24
        CWRUsWest2 -> 25
        CloudWatchRegion' name -> (error . showText) $ "Unknown CloudWatchRegion: " <> original name

-- | Represents the bounds of /known/ $CloudWatchRegion.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CloudWatchRegion where
    minBound = CWRAfSouth1
    maxBound = CWRUsWest2

instance Hashable     CloudWatchRegion
instance NFData       CloudWatchRegion
instance ToByteString CloudWatchRegion
instance ToQuery      CloudWatchRegion
instance ToHeader     CloudWatchRegion

instance FromXML CloudWatchRegion where
    parseXML = parseXMLText "CloudWatchRegion"

instance ToXML CloudWatchRegion where
    toXML = toXMLText
