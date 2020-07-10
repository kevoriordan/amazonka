{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.ServiceNamespace
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApplicationAutoScaling.Types.ServiceNamespace (
  ServiceNamespace (
    ..
    , Appstream
    , Cassandra
    , Comprehend
    , CustomResource
    , Dynamodb
    , EC2
    , Ecs
    , Elasticmapreduce
    , Lambda
    , RDS
    , Sagemaker
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ServiceNamespace = ServiceNamespace' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern Appstream :: ServiceNamespace
pattern Appstream = ServiceNamespace' "appstream"

pattern Cassandra :: ServiceNamespace
pattern Cassandra = ServiceNamespace' "cassandra"

pattern Comprehend :: ServiceNamespace
pattern Comprehend = ServiceNamespace' "comprehend"

pattern CustomResource :: ServiceNamespace
pattern CustomResource = ServiceNamespace' "custom-resource"

pattern Dynamodb :: ServiceNamespace
pattern Dynamodb = ServiceNamespace' "dynamodb"

pattern EC2 :: ServiceNamespace
pattern EC2 = ServiceNamespace' "ec2"

pattern Ecs :: ServiceNamespace
pattern Ecs = ServiceNamespace' "ecs"

pattern Elasticmapreduce :: ServiceNamespace
pattern Elasticmapreduce = ServiceNamespace' "elasticmapreduce"

pattern Lambda :: ServiceNamespace
pattern Lambda = ServiceNamespace' "lambda"

pattern RDS :: ServiceNamespace
pattern RDS = ServiceNamespace' "rds"

pattern Sagemaker :: ServiceNamespace
pattern Sagemaker = ServiceNamespace' "sagemaker"

{-# COMPLETE
  Appstream,
  Cassandra,
  Comprehend,
  CustomResource,
  Dynamodb,
  EC2,
  Ecs,
  Elasticmapreduce,
  Lambda,
  RDS,
  Sagemaker,
  ServiceNamespace' #-}

instance FromText ServiceNamespace where
    parser = (ServiceNamespace' . mk) <$> takeText

instance ToText ServiceNamespace where
    toText (ServiceNamespace' ci) = original ci

-- | Represents an enum of /known/ $ServiceNamespace.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ServiceNamespace where
    toEnum i = case i of
        0 -> Appstream
        1 -> Cassandra
        2 -> Comprehend
        3 -> CustomResource
        4 -> Dynamodb
        5 -> EC2
        6 -> Ecs
        7 -> Elasticmapreduce
        8 -> Lambda
        9 -> RDS
        10 -> Sagemaker
        _ -> (error . showText) $ "Unknown index for ServiceNamespace: " <> toText i
    fromEnum x = case x of
        Appstream -> 0
        Cassandra -> 1
        Comprehend -> 2
        CustomResource -> 3
        Dynamodb -> 4
        EC2 -> 5
        Ecs -> 6
        Elasticmapreduce -> 7
        Lambda -> 8
        RDS -> 9
        Sagemaker -> 10
        ServiceNamespace' name -> (error . showText) $ "Unknown ServiceNamespace: " <> original name

-- | Represents the bounds of /known/ $ServiceNamespace.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ServiceNamespace where
    minBound = Appstream
    maxBound = Sagemaker

instance Hashable     ServiceNamespace
instance NFData       ServiceNamespace
instance ToByteString ServiceNamespace
instance ToQuery      ServiceNamespace
instance ToHeader     ServiceNamespace

instance ToJSON ServiceNamespace where
    toJSON = toJSONText

instance FromJSON ServiceNamespace where
    parseJSON = parseJSONText "ServiceNamespace"
