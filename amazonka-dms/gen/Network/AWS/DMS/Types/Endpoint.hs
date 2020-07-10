{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.Endpoint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.Endpoint where

import Network.AWS.DMS.Types.DmsSSLModeValue
import Network.AWS.DMS.Types.DmsTransferSettings
import Network.AWS.DMS.Types.DynamoDBSettings
import Network.AWS.DMS.Types.ElasticsearchSettings
import Network.AWS.DMS.Types.KafkaSettings
import Network.AWS.DMS.Types.KinesisSettings
import Network.AWS.DMS.Types.MongoDBSettings
import Network.AWS.DMS.Types.NeptuneSettings
import Network.AWS.DMS.Types.RedshiftSettings
import Network.AWS.DMS.Types.ReplicationEndpointTypeValue
import Network.AWS.DMS.Types.S3Settings
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an endpoint of a database instance in response to operations such as the following:
--
--
--     * @CreateEndpoint@ 
--
--     * @DescribeEndpoint@ 
--
--     * @DescribeEndpointTypes@ 
--
--     * @ModifyEndpoint@ 
--
--
--
--
-- /See:/ 'endpoint' smart constructor.
data Endpoint = Endpoint'{_eStatus :: !(Maybe Text),
                          _eDmsTransferSettings ::
                          !(Maybe DmsTransferSettings),
                          _eServerName :: !(Maybe Text),
                          _eCertificateARN :: !(Maybe Text),
                          _eServiceAccessRoleARN :: !(Maybe Text),
                          _eEngineDisplayName :: !(Maybe Text),
                          _eExtraConnectionAttributes :: !(Maybe Text),
                          _eKafkaSettings :: !(Maybe KafkaSettings),
                          _eEndpointType ::
                          !(Maybe ReplicationEndpointTypeValue),
                          _eRedshiftSettings :: !(Maybe RedshiftSettings),
                          _eElasticsearchSettings ::
                          !(Maybe ElasticsearchSettings),
                          _eUsername :: !(Maybe Text),
                          _eExternalTableDefinition :: !(Maybe Text),
                          _eEngineName :: !(Maybe Text),
                          _eNeptuneSettings :: !(Maybe NeptuneSettings),
                          _eKMSKeyId :: !(Maybe Text),
                          _eMongoDBSettings :: !(Maybe MongoDBSettings),
                          _eSSLMode :: !(Maybe DmsSSLModeValue),
                          _eDatabaseName :: !(Maybe Text),
                          _eS3Settings :: !(Maybe S3Settings),
                          _eKinesisSettings :: !(Maybe KinesisSettings),
                          _eEndpointIdentifier :: !(Maybe Text),
                          _eExternalId :: !(Maybe Text),
                          _eDynamoDBSettings :: !(Maybe DynamoDBSettings),
                          _eEndpointARN :: !(Maybe Text),
                          _ePort :: !(Maybe Int)}
                  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'Endpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eStatus' - The status of the endpoint.
--
-- * 'eDmsTransferSettings' - The settings in JSON format for the DMS transfer type of source endpoint.  Possible settings include the following:     * @ServiceAccessRoleArn@ - The IAM role that has permission to access the Amazon S3 bucket.     * @BucketName@ - The name of the S3 bucket to use.     * @CompressionType@ - An optional parameter to use GZIP to compress the target files. To use GZIP, set this value to @NONE@ (the default). To keep the files uncompressed, don't use this value. Shorthand syntax for these settings is as follows: @ServiceAccessRoleArn=string,BucketName=string,CompressionType=string@  JSON syntax for these settings is as follows: @{ "ServiceAccessRoleArn": "string", "BucketName": "string", "CompressionType": "none"|"gzip" } @ 
--
-- * 'eServerName' - The name of the server at the endpoint.
--
-- * 'eCertificateARN' - The Amazon Resource Name (ARN) used for SSL connection to the endpoint.
--
-- * 'eServiceAccessRoleARN' - The Amazon Resource Name (ARN) used by the service access IAM role.
--
-- * 'eEngineDisplayName' - The expanded name for the engine name. For example, if the @EngineName@ parameter is "aurora," this value would be "Amazon Aurora MySQL."
--
-- * 'eExtraConnectionAttributes' - Additional connection attributes used to connect to the endpoint.
--
-- * 'eKafkaSettings' - The settings for the Apache Kafka target endpoint. For more information, see the @KafkaSettings@ structure.
--
-- * 'eEndpointType' - The type of endpoint. Valid values are @source@ and @target@ .
--
-- * 'eRedshiftSettings' - Settings for the Amazon Redshift endpoint.
--
-- * 'eElasticsearchSettings' - The settings for the Elasticsearch source endpoint. For more information, see the @ElasticsearchSettings@ structure.
--
-- * 'eUsername' - The user name used to connect to the endpoint.
--
-- * 'eExternalTableDefinition' - The external table definition.
--
-- * 'eEngineName' - The database engine name. Valid values, depending on the EndpointType, include @"mysql"@ , @"oracle"@ , @"postgres"@ , @"mariadb"@ , @"aurora"@ , @"aurora-postgresql"@ , @"redshift"@ , @"s3"@ , @"db2"@ , @"azuredb"@ , @"sybase"@ , @"dynamodb"@ , @"mongodb"@ , @"kinesis"@ , @"kafka"@ , @"elasticsearch"@ , @"documentdb"@ , and @"sqlserver"@ .
--
-- * 'eNeptuneSettings' - The settings for the MongoDB source endpoint. For more information, see the @NeptuneSettings@ structure.
--
-- * 'eKMSKeyId' - An AWS KMS key identifier that is used to encrypt the connection parameters for the endpoint. If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- * 'eMongoDBSettings' - The settings for the MongoDB source endpoint. For more information, see the @MongoDbSettings@ structure.
--
-- * 'eSSLMode' - The SSL mode used to connect to the endpoint. The default value is @none@ .
--
-- * 'eDatabaseName' - The name of the database at the endpoint.
--
-- * 'eS3Settings' - The settings for the S3 target endpoint. For more information, see the @S3Settings@ structure.
--
-- * 'eKinesisSettings' - The settings for the Amazon Kinesis target endpoint. For more information, see the @KinesisSettings@ structure.
--
-- * 'eEndpointIdentifier' - The database endpoint identifier. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen or contain two consecutive hyphens.
--
-- * 'eExternalId' - Value returned by a call to CreateEndpoint that can be used for cross-account validation. Use it on a subsequent call to CreateEndpoint to create the endpoint with a cross-account. 
--
-- * 'eDynamoDBSettings' - The settings for the target DynamoDB database. For more information, see the @DynamoDBSettings@ structure.
--
-- * 'eEndpointARN' - The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- * 'ePort' - The port value used to access the endpoint.
endpoint
    :: Endpoint
endpoint
  = Endpoint'{_eStatus = Nothing,
              _eDmsTransferSettings = Nothing,
              _eServerName = Nothing, _eCertificateARN = Nothing,
              _eServiceAccessRoleARN = Nothing,
              _eEngineDisplayName = Nothing,
              _eExtraConnectionAttributes = Nothing,
              _eKafkaSettings = Nothing, _eEndpointType = Nothing,
              _eRedshiftSettings = Nothing,
              _eElasticsearchSettings = Nothing,
              _eUsername = Nothing,
              _eExternalTableDefinition = Nothing,
              _eEngineName = Nothing, _eNeptuneSettings = Nothing,
              _eKMSKeyId = Nothing, _eMongoDBSettings = Nothing,
              _eSSLMode = Nothing, _eDatabaseName = Nothing,
              _eS3Settings = Nothing, _eKinesisSettings = Nothing,
              _eEndpointIdentifier = Nothing,
              _eExternalId = Nothing, _eDynamoDBSettings = Nothing,
              _eEndpointARN = Nothing, _ePort = Nothing}

-- | The status of the endpoint.
eStatus :: Lens' Endpoint (Maybe Text)
eStatus = lens _eStatus (\ s a -> s{_eStatus = a})

-- | The settings in JSON format for the DMS transfer type of source endpoint.  Possible settings include the following:     * @ServiceAccessRoleArn@ - The IAM role that has permission to access the Amazon S3 bucket.     * @BucketName@ - The name of the S3 bucket to use.     * @CompressionType@ - An optional parameter to use GZIP to compress the target files. To use GZIP, set this value to @NONE@ (the default). To keep the files uncompressed, don't use this value. Shorthand syntax for these settings is as follows: @ServiceAccessRoleArn=string,BucketName=string,CompressionType=string@  JSON syntax for these settings is as follows: @{ "ServiceAccessRoleArn": "string", "BucketName": "string", "CompressionType": "none"|"gzip" } @ 
eDmsTransferSettings :: Lens' Endpoint (Maybe DmsTransferSettings)
eDmsTransferSettings = lens _eDmsTransferSettings (\ s a -> s{_eDmsTransferSettings = a})

-- | The name of the server at the endpoint.
eServerName :: Lens' Endpoint (Maybe Text)
eServerName = lens _eServerName (\ s a -> s{_eServerName = a})

-- | The Amazon Resource Name (ARN) used for SSL connection to the endpoint.
eCertificateARN :: Lens' Endpoint (Maybe Text)
eCertificateARN = lens _eCertificateARN (\ s a -> s{_eCertificateARN = a})

-- | The Amazon Resource Name (ARN) used by the service access IAM role.
eServiceAccessRoleARN :: Lens' Endpoint (Maybe Text)
eServiceAccessRoleARN = lens _eServiceAccessRoleARN (\ s a -> s{_eServiceAccessRoleARN = a})

-- | The expanded name for the engine name. For example, if the @EngineName@ parameter is "aurora," this value would be "Amazon Aurora MySQL."
eEngineDisplayName :: Lens' Endpoint (Maybe Text)
eEngineDisplayName = lens _eEngineDisplayName (\ s a -> s{_eEngineDisplayName = a})

-- | Additional connection attributes used to connect to the endpoint.
eExtraConnectionAttributes :: Lens' Endpoint (Maybe Text)
eExtraConnectionAttributes = lens _eExtraConnectionAttributes (\ s a -> s{_eExtraConnectionAttributes = a})

-- | The settings for the Apache Kafka target endpoint. For more information, see the @KafkaSettings@ structure.
eKafkaSettings :: Lens' Endpoint (Maybe KafkaSettings)
eKafkaSettings = lens _eKafkaSettings (\ s a -> s{_eKafkaSettings = a})

-- | The type of endpoint. Valid values are @source@ and @target@ .
eEndpointType :: Lens' Endpoint (Maybe ReplicationEndpointTypeValue)
eEndpointType = lens _eEndpointType (\ s a -> s{_eEndpointType = a})

-- | Settings for the Amazon Redshift endpoint.
eRedshiftSettings :: Lens' Endpoint (Maybe RedshiftSettings)
eRedshiftSettings = lens _eRedshiftSettings (\ s a -> s{_eRedshiftSettings = a})

-- | The settings for the Elasticsearch source endpoint. For more information, see the @ElasticsearchSettings@ structure.
eElasticsearchSettings :: Lens' Endpoint (Maybe ElasticsearchSettings)
eElasticsearchSettings = lens _eElasticsearchSettings (\ s a -> s{_eElasticsearchSettings = a})

-- | The user name used to connect to the endpoint.
eUsername :: Lens' Endpoint (Maybe Text)
eUsername = lens _eUsername (\ s a -> s{_eUsername = a})

-- | The external table definition.
eExternalTableDefinition :: Lens' Endpoint (Maybe Text)
eExternalTableDefinition = lens _eExternalTableDefinition (\ s a -> s{_eExternalTableDefinition = a})

-- | The database engine name. Valid values, depending on the EndpointType, include @"mysql"@ , @"oracle"@ , @"postgres"@ , @"mariadb"@ , @"aurora"@ , @"aurora-postgresql"@ , @"redshift"@ , @"s3"@ , @"db2"@ , @"azuredb"@ , @"sybase"@ , @"dynamodb"@ , @"mongodb"@ , @"kinesis"@ , @"kafka"@ , @"elasticsearch"@ , @"documentdb"@ , and @"sqlserver"@ .
eEngineName :: Lens' Endpoint (Maybe Text)
eEngineName = lens _eEngineName (\ s a -> s{_eEngineName = a})

-- | The settings for the MongoDB source endpoint. For more information, see the @NeptuneSettings@ structure.
eNeptuneSettings :: Lens' Endpoint (Maybe NeptuneSettings)
eNeptuneSettings = lens _eNeptuneSettings (\ s a -> s{_eNeptuneSettings = a})

-- | An AWS KMS key identifier that is used to encrypt the connection parameters for the endpoint. If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
eKMSKeyId :: Lens' Endpoint (Maybe Text)
eKMSKeyId = lens _eKMSKeyId (\ s a -> s{_eKMSKeyId = a})

-- | The settings for the MongoDB source endpoint. For more information, see the @MongoDbSettings@ structure.
eMongoDBSettings :: Lens' Endpoint (Maybe MongoDBSettings)
eMongoDBSettings = lens _eMongoDBSettings (\ s a -> s{_eMongoDBSettings = a})

-- | The SSL mode used to connect to the endpoint. The default value is @none@ .
eSSLMode :: Lens' Endpoint (Maybe DmsSSLModeValue)
eSSLMode = lens _eSSLMode (\ s a -> s{_eSSLMode = a})

-- | The name of the database at the endpoint.
eDatabaseName :: Lens' Endpoint (Maybe Text)
eDatabaseName = lens _eDatabaseName (\ s a -> s{_eDatabaseName = a})

-- | The settings for the S3 target endpoint. For more information, see the @S3Settings@ structure.
eS3Settings :: Lens' Endpoint (Maybe S3Settings)
eS3Settings = lens _eS3Settings (\ s a -> s{_eS3Settings = a})

-- | The settings for the Amazon Kinesis target endpoint. For more information, see the @KinesisSettings@ structure.
eKinesisSettings :: Lens' Endpoint (Maybe KinesisSettings)
eKinesisSettings = lens _eKinesisSettings (\ s a -> s{_eKinesisSettings = a})

-- | The database endpoint identifier. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen or contain two consecutive hyphens.
eEndpointIdentifier :: Lens' Endpoint (Maybe Text)
eEndpointIdentifier = lens _eEndpointIdentifier (\ s a -> s{_eEndpointIdentifier = a})

-- | Value returned by a call to CreateEndpoint that can be used for cross-account validation. Use it on a subsequent call to CreateEndpoint to create the endpoint with a cross-account. 
eExternalId :: Lens' Endpoint (Maybe Text)
eExternalId = lens _eExternalId (\ s a -> s{_eExternalId = a})

-- | The settings for the target DynamoDB database. For more information, see the @DynamoDBSettings@ structure.
eDynamoDBSettings :: Lens' Endpoint (Maybe DynamoDBSettings)
eDynamoDBSettings = lens _eDynamoDBSettings (\ s a -> s{_eDynamoDBSettings = a})

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
eEndpointARN :: Lens' Endpoint (Maybe Text)
eEndpointARN = lens _eEndpointARN (\ s a -> s{_eEndpointARN = a})

-- | The port value used to access the endpoint.
ePort :: Lens' Endpoint (Maybe Int)
ePort = lens _ePort (\ s a -> s{_ePort = a})

instance FromJSON Endpoint where
        parseJSON
          = withObject "Endpoint"
              (\ x ->
                 Endpoint' <$>
                   (x .:? "Status") <*> (x .:? "DmsTransferSettings")
                     <*> (x .:? "ServerName")
                     <*> (x .:? "CertificateArn")
                     <*> (x .:? "ServiceAccessRoleArn")
                     <*> (x .:? "EngineDisplayName")
                     <*> (x .:? "ExtraConnectionAttributes")
                     <*> (x .:? "KafkaSettings")
                     <*> (x .:? "EndpointType")
                     <*> (x .:? "RedshiftSettings")
                     <*> (x .:? "ElasticsearchSettings")
                     <*> (x .:? "Username")
                     <*> (x .:? "ExternalTableDefinition")
                     <*> (x .:? "EngineName")
                     <*> (x .:? "NeptuneSettings")
                     <*> (x .:? "KmsKeyId")
                     <*> (x .:? "MongoDbSettings")
                     <*> (x .:? "SslMode")
                     <*> (x .:? "DatabaseName")
                     <*> (x .:? "S3Settings")
                     <*> (x .:? "KinesisSettings")
                     <*> (x .:? "EndpointIdentifier")
                     <*> (x .:? "ExternalId")
                     <*> (x .:? "DynamoDbSettings")
                     <*> (x .:? "EndpointArn")
                     <*> (x .:? "Port"))

instance Hashable Endpoint where

instance NFData Endpoint where
