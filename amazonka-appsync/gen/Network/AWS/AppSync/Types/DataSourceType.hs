{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.DataSourceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.DataSourceType (
  DataSourceType (
    ..
    , AWSLambda
    , AmazonDynamodb
    , AmazonElasticsearch
    , HTTP
    , None
    , RelationalDatabase
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DataSourceType = DataSourceType' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern AWSLambda :: DataSourceType
pattern AWSLambda = DataSourceType' "AWS_LAMBDA"

pattern AmazonDynamodb :: DataSourceType
pattern AmazonDynamodb = DataSourceType' "AMAZON_DYNAMODB"

pattern AmazonElasticsearch :: DataSourceType
pattern AmazonElasticsearch = DataSourceType' "AMAZON_ELASTICSEARCH"

pattern HTTP :: DataSourceType
pattern HTTP = DataSourceType' "HTTP"

pattern None :: DataSourceType
pattern None = DataSourceType' "NONE"

pattern RelationalDatabase :: DataSourceType
pattern RelationalDatabase = DataSourceType' "RELATIONAL_DATABASE"

{-# COMPLETE
  AWSLambda,
  AmazonDynamodb,
  AmazonElasticsearch,
  HTTP,
  None,
  RelationalDatabase,
  DataSourceType' #-}

instance FromText DataSourceType where
    parser = (DataSourceType' . mk) <$> takeText

instance ToText DataSourceType where
    toText (DataSourceType' ci) = original ci

-- | Represents an enum of /known/ $DataSourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DataSourceType where
    toEnum i = case i of
        0 -> AWSLambda
        1 -> AmazonDynamodb
        2 -> AmazonElasticsearch
        3 -> HTTP
        4 -> None
        5 -> RelationalDatabase
        _ -> (error . showText) $ "Unknown index for DataSourceType: " <> toText i
    fromEnum x = case x of
        AWSLambda -> 0
        AmazonDynamodb -> 1
        AmazonElasticsearch -> 2
        HTTP -> 3
        None -> 4
        RelationalDatabase -> 5
        DataSourceType' name -> (error . showText) $ "Unknown DataSourceType: " <> original name

-- | Represents the bounds of /known/ $DataSourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DataSourceType where
    minBound = AWSLambda
    maxBound = RelationalDatabase

instance Hashable     DataSourceType
instance NFData       DataSourceType
instance ToByteString DataSourceType
instance ToQuery      DataSourceType
instance ToHeader     DataSourceType

instance ToJSON DataSourceType where
    toJSON = toJSONText

instance FromJSON DataSourceType where
    parseJSON = parseJSONText "DataSourceType"
