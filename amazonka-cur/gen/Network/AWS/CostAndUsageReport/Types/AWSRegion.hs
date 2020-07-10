{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.Types.AWSRegion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostAndUsageReport.Types.AWSRegion (
  AWSRegion (
    ..
    , ApEast1
    , ApNortheast1
    , ApNortheast3
    , ApSoutheast1
    , ApSoutheast2
    , EuCentral1
    , EuNorth1
    , EuWest1
    , UsEast1
    , UsWest1
    , UsWest2
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The region of the S3 bucket that AWS delivers the report into.
--
--
data AWSRegion = AWSRegion' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern ApEast1 :: AWSRegion
pattern ApEast1 = AWSRegion' "ap-east-1"

pattern ApNortheast1 :: AWSRegion
pattern ApNortheast1 = AWSRegion' "ap-northeast-1"

pattern ApNortheast3 :: AWSRegion
pattern ApNortheast3 = AWSRegion' "ap-northeast-3"

pattern ApSoutheast1 :: AWSRegion
pattern ApSoutheast1 = AWSRegion' "ap-southeast-1"

pattern ApSoutheast2 :: AWSRegion
pattern ApSoutheast2 = AWSRegion' "ap-southeast-2"

pattern EuCentral1 :: AWSRegion
pattern EuCentral1 = AWSRegion' "eu-central-1"

pattern EuNorth1 :: AWSRegion
pattern EuNorth1 = AWSRegion' "eu-north-1"

pattern EuWest1 :: AWSRegion
pattern EuWest1 = AWSRegion' "eu-west-1"

pattern UsEast1 :: AWSRegion
pattern UsEast1 = AWSRegion' "us-east-1"

pattern UsWest1 :: AWSRegion
pattern UsWest1 = AWSRegion' "us-west-1"

pattern UsWest2 :: AWSRegion
pattern UsWest2 = AWSRegion' "us-west-2"

{-# COMPLETE
  ApEast1,
  ApNortheast1,
  ApNortheast3,
  ApSoutheast1,
  ApSoutheast2,
  EuCentral1,
  EuNorth1,
  EuWest1,
  UsEast1,
  UsWest1,
  UsWest2,
  AWSRegion' #-}

instance FromText AWSRegion where
    parser = (AWSRegion' . mk) <$> takeText

instance ToText AWSRegion where
    toText (AWSRegion' ci) = original ci

-- | Represents an enum of /known/ $AWSRegion.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AWSRegion where
    toEnum i = case i of
        0 -> ApEast1
        1 -> ApNortheast1
        2 -> ApNortheast3
        3 -> ApSoutheast1
        4 -> ApSoutheast2
        5 -> EuCentral1
        6 -> EuNorth1
        7 -> EuWest1
        8 -> UsEast1
        9 -> UsWest1
        10 -> UsWest2
        _ -> (error . showText) $ "Unknown index for AWSRegion: " <> toText i
    fromEnum x = case x of
        ApEast1 -> 0
        ApNortheast1 -> 1
        ApNortheast3 -> 2
        ApSoutheast1 -> 3
        ApSoutheast2 -> 4
        EuCentral1 -> 5
        EuNorth1 -> 6
        EuWest1 -> 7
        UsEast1 -> 8
        UsWest1 -> 9
        UsWest2 -> 10
        AWSRegion' name -> (error . showText) $ "Unknown AWSRegion: " <> original name

-- | Represents the bounds of /known/ $AWSRegion.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AWSRegion where
    minBound = ApEast1
    maxBound = UsWest2

instance Hashable     AWSRegion
instance NFData       AWSRegion
instance ToByteString AWSRegion
instance ToQuery      AWSRegion
instance ToHeader     AWSRegion

instance ToJSON AWSRegion where
    toJSON = toJSONText

instance FromJSON AWSRegion where
    parseJSON = parseJSONText "AWSRegion"
