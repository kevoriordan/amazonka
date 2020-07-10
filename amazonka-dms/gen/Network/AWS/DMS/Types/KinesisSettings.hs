{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.KinesisSettings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.KinesisSettings where

import Network.AWS.DMS.Types.MessageFormatValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information that describes an Amazon Kinesis Data Stream endpoint. This information includes the output format of records applied to the endpoint and details of transaction and control table data information.
--
--
--
-- /See:/ 'kinesisSettings' smart constructor.
data KinesisSettings = KinesisSettings'{_ksIncludeTransactionDetails
                                        :: !(Maybe Bool),
                                        _ksIncludeTableAlterOperations ::
                                        !(Maybe Bool),
                                        _ksServiceAccessRoleARN ::
                                        !(Maybe Text),
                                        _ksPartitionIncludeSchemaTable ::
                                        !(Maybe Bool),
                                        _ksStreamARN :: !(Maybe Text),
                                        _ksIncludeControlDetails ::
                                        !(Maybe Bool),
                                        _ksIncludePartitionValue ::
                                        !(Maybe Bool),
                                        _ksMessageFormat ::
                                        !(Maybe MessageFormatValue)}
                         deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KinesisSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ksIncludeTransactionDetails' - Provides detailed transaction information from the source database. This information includes a commit timestamp, a log position, and values for @transaction_id@ , previous @transaction_id@ , and @transaction_record_id@ (the record offset within a transaction). The default is @False@ .
--
-- * 'ksIncludeTableAlterOperations' - Includes any data definition language (DDL) operations that change the table in the control data, such as @rename-table@ , @drop-table@ , @add-column@ , @drop-column@ , and @rename-column@ . The default is @False@ .
--
-- * 'ksServiceAccessRoleARN' - The Amazon Resource Name (ARN) for the AWS Identity and Access Management (IAM) role that AWS DMS uses to write to the Kinesis data stream.
--
-- * 'ksPartitionIncludeSchemaTable' - Prefixes schema and table names to partition values, when the partition type is @primary-key-type@ . Doing this increases data distribution among Kinesis shards. For example, suppose that a SysBench schema has thousands of tables and each table has only limited range for a primary key. In this case, the same primary key is sent from thousands of tables to the same shard, which causes throttling. The default is @False@ .
--
-- * 'ksStreamARN' - The Amazon Resource Name (ARN) for the Amazon Kinesis Data Streams endpoint.
--
-- * 'ksIncludeControlDetails' - Shows detailed control information for table definition, column definition, and table and column changes in the Kinesis message output. The default is @False@ .
--
-- * 'ksIncludePartitionValue' - Shows the partition value within the Kinesis message output, unless the partition type is @schema-table-type@ . The default is @False@ .
--
-- * 'ksMessageFormat' - The output format for the records created on the endpoint. The message format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no tab).
kinesisSettings
    :: KinesisSettings
kinesisSettings
  = KinesisSettings'{_ksIncludeTransactionDetails =
                       Nothing,
                     _ksIncludeTableAlterOperations = Nothing,
                     _ksServiceAccessRoleARN = Nothing,
                     _ksPartitionIncludeSchemaTable = Nothing,
                     _ksStreamARN = Nothing,
                     _ksIncludeControlDetails = Nothing,
                     _ksIncludePartitionValue = Nothing,
                     _ksMessageFormat = Nothing}

-- | Provides detailed transaction information from the source database. This information includes a commit timestamp, a log position, and values for @transaction_id@ , previous @transaction_id@ , and @transaction_record_id@ (the record offset within a transaction). The default is @False@ .
ksIncludeTransactionDetails :: Lens' KinesisSettings (Maybe Bool)
ksIncludeTransactionDetails = lens _ksIncludeTransactionDetails (\ s a -> s{_ksIncludeTransactionDetails = a})

-- | Includes any data definition language (DDL) operations that change the table in the control data, such as @rename-table@ , @drop-table@ , @add-column@ , @drop-column@ , and @rename-column@ . The default is @False@ .
ksIncludeTableAlterOperations :: Lens' KinesisSettings (Maybe Bool)
ksIncludeTableAlterOperations = lens _ksIncludeTableAlterOperations (\ s a -> s{_ksIncludeTableAlterOperations = a})

-- | The Amazon Resource Name (ARN) for the AWS Identity and Access Management (IAM) role that AWS DMS uses to write to the Kinesis data stream.
ksServiceAccessRoleARN :: Lens' KinesisSettings (Maybe Text)
ksServiceAccessRoleARN = lens _ksServiceAccessRoleARN (\ s a -> s{_ksServiceAccessRoleARN = a})

-- | Prefixes schema and table names to partition values, when the partition type is @primary-key-type@ . Doing this increases data distribution among Kinesis shards. For example, suppose that a SysBench schema has thousands of tables and each table has only limited range for a primary key. In this case, the same primary key is sent from thousands of tables to the same shard, which causes throttling. The default is @False@ .
ksPartitionIncludeSchemaTable :: Lens' KinesisSettings (Maybe Bool)
ksPartitionIncludeSchemaTable = lens _ksPartitionIncludeSchemaTable (\ s a -> s{_ksPartitionIncludeSchemaTable = a})

-- | The Amazon Resource Name (ARN) for the Amazon Kinesis Data Streams endpoint.
ksStreamARN :: Lens' KinesisSettings (Maybe Text)
ksStreamARN = lens _ksStreamARN (\ s a -> s{_ksStreamARN = a})

-- | Shows detailed control information for table definition, column definition, and table and column changes in the Kinesis message output. The default is @False@ .
ksIncludeControlDetails :: Lens' KinesisSettings (Maybe Bool)
ksIncludeControlDetails = lens _ksIncludeControlDetails (\ s a -> s{_ksIncludeControlDetails = a})

-- | Shows the partition value within the Kinesis message output, unless the partition type is @schema-table-type@ . The default is @False@ .
ksIncludePartitionValue :: Lens' KinesisSettings (Maybe Bool)
ksIncludePartitionValue = lens _ksIncludePartitionValue (\ s a -> s{_ksIncludePartitionValue = a})

-- | The output format for the records created on the endpoint. The message format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no tab).
ksMessageFormat :: Lens' KinesisSettings (Maybe MessageFormatValue)
ksMessageFormat = lens _ksMessageFormat (\ s a -> s{_ksMessageFormat = a})

instance FromJSON KinesisSettings where
        parseJSON
          = withObject "KinesisSettings"
              (\ x ->
                 KinesisSettings' <$>
                   (x .:? "IncludeTransactionDetails") <*>
                     (x .:? "IncludeTableAlterOperations")
                     <*> (x .:? "ServiceAccessRoleArn")
                     <*> (x .:? "PartitionIncludeSchemaTable")
                     <*> (x .:? "StreamArn")
                     <*> (x .:? "IncludeControlDetails")
                     <*> (x .:? "IncludePartitionValue")
                     <*> (x .:? "MessageFormat"))

instance Hashable KinesisSettings where

instance NFData KinesisSettings where

instance ToJSON KinesisSettings where
        toJSON KinesisSettings'{..}
          = object
              (catMaybes
                 [("IncludeTransactionDetails" .=) <$>
                    _ksIncludeTransactionDetails,
                  ("IncludeTableAlterOperations" .=) <$>
                    _ksIncludeTableAlterOperations,
                  ("ServiceAccessRoleArn" .=) <$>
                    _ksServiceAccessRoleARN,
                  ("PartitionIncludeSchemaTable" .=) <$>
                    _ksPartitionIncludeSchemaTable,
                  ("StreamArn" .=) <$> _ksStreamARN,
                  ("IncludeControlDetails" .=) <$>
                    _ksIncludeControlDetails,
                  ("IncludePartitionValue" .=) <$>
                    _ksIncludePartitionValue,
                  ("MessageFormat" .=) <$> _ksMessageFormat])
