{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.GlueConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.GlueConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration information for coordination with the AWS Glue ETL (extract, transform and load) service.
--
--
--
-- /See:/ 'glueConfiguration' smart constructor.
data GlueConfiguration = GlueConfiguration'{_gcTableName
                                            :: !Text,
                                            _gcDatabaseName :: !Text}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GlueConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcTableName' - The name of the table in your AWS Glue Data Catalog which is used to perform the ETL (extract, transform and load) operations. (An AWS Glue Data Catalog table contains partitioned data and descriptions of data sources and targets.)
--
-- * 'gcDatabaseName' - The name of the database in your AWS Glue Data Catalog in which the table is located. (An AWS Glue Data Catalog database contains Glue Data tables.)
glueConfiguration
    :: Text -- ^ 'gcTableName'
    -> Text -- ^ 'gcDatabaseName'
    -> GlueConfiguration
glueConfiguration pTableName_ pDatabaseName_
  = GlueConfiguration'{_gcTableName = pTableName_,
                       _gcDatabaseName = pDatabaseName_}

-- | The name of the table in your AWS Glue Data Catalog which is used to perform the ETL (extract, transform and load) operations. (An AWS Glue Data Catalog table contains partitioned data and descriptions of data sources and targets.)
gcTableName :: Lens' GlueConfiguration Text
gcTableName = lens _gcTableName (\ s a -> s{_gcTableName = a})

-- | The name of the database in your AWS Glue Data Catalog in which the table is located. (An AWS Glue Data Catalog database contains Glue Data tables.)
gcDatabaseName :: Lens' GlueConfiguration Text
gcDatabaseName = lens _gcDatabaseName (\ s a -> s{_gcDatabaseName = a})

instance FromJSON GlueConfiguration where
        parseJSON
          = withObject "GlueConfiguration"
              (\ x ->
                 GlueConfiguration' <$>
                   (x .: "tableName") <*> (x .: "databaseName"))

instance Hashable GlueConfiguration where

instance NFData GlueConfiguration where

instance ToJSON GlueConfiguration where
        toJSON GlueConfiguration'{..}
          = object
              (catMaybes
                 [Just ("tableName" .= _gcTableName),
                  Just ("databaseName" .= _gcDatabaseName)])
