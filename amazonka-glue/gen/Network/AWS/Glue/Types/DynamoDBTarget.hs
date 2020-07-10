{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DynamoDBTarget
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.DynamoDBTarget where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies an Amazon DynamoDB table to crawl.
--
--
--
-- /See:/ 'dynamoDBTarget' smart constructor.
newtype DynamoDBTarget = DynamoDBTarget'{_ddtPath ::
                                         Maybe Text}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DynamoDBTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddtPath' - The name of the DynamoDB table to crawl.
dynamoDBTarget
    :: DynamoDBTarget
dynamoDBTarget = DynamoDBTarget'{_ddtPath = Nothing}

-- | The name of the DynamoDB table to crawl.
ddtPath :: Lens' DynamoDBTarget (Maybe Text)
ddtPath = lens _ddtPath (\ s a -> s{_ddtPath = a})

instance FromJSON DynamoDBTarget where
        parseJSON
          = withObject "DynamoDBTarget"
              (\ x -> DynamoDBTarget' <$> (x .:? "Path"))

instance Hashable DynamoDBTarget where

instance NFData DynamoDBTarget where

instance ToJSON DynamoDBTarget where
        toJSON DynamoDBTarget'{..}
          = object (catMaybes [("Path" .=) <$> _ddtPath])
