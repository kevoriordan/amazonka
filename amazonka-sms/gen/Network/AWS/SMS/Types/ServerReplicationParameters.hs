{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ServerReplicationParameters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types.ServerReplicationParameters where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SMS.Types.LicenseType

-- | Replication parameters for replicating a server.
--
--
--
-- /See:/ 'serverReplicationParameters' smart constructor.
data ServerReplicationParameters = ServerReplicationParameters'{_srpFrequency
                                                                :: !(Maybe Int),
                                                                _srpNumberOfRecentAMIsToKeep
                                                                :: !(Maybe Int),
                                                                _srpSeedTime ::
                                                                !(Maybe POSIX),
                                                                _srpLicenseType
                                                                ::
                                                                !(Maybe
                                                                    LicenseType),
                                                                _srpEncrypted ::
                                                                !(Maybe Bool),
                                                                _srpKmsKeyId ::
                                                                !(Maybe Text),
                                                                _srpRunOnce ::
                                                                !(Maybe Bool)}
                                     deriving (Eq, Read, Show, Data, Typeable,
                                               Generic)

-- | Creates a value of 'ServerReplicationParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srpFrequency' - Frequency of creating replication jobs for the server.
--
-- * 'srpNumberOfRecentAMIsToKeep' - Number of recent AMIs to keep when creating a replication job for this server.
--
-- * 'srpSeedTime' - Seed time for creating a replication job for the server.
--
-- * 'srpLicenseType' - License type for creating a replication job for the server.
--
-- * 'srpEncrypted' - When true, the replication job produces encrypted AMIs. See also @KmsKeyId@ below.
--
-- * 'srpKmsKeyId' -  KMS key ID for replication jobs that produce encrypted AMIs. Can be any of the following:      * KMS key ID     * KMS key alias     * ARN referring to KMS key ID     * ARN referring to KMS key alias If encrypted is /true/ but a KMS key id is not specified, the customer's default KMS key for EBS is used. 
--
-- * 'srpRunOnce' - 
serverReplicationParameters
    :: ServerReplicationParameters
serverReplicationParameters
  = ServerReplicationParameters'{_srpFrequency =
                                   Nothing,
                                 _srpNumberOfRecentAMIsToKeep = Nothing,
                                 _srpSeedTime = Nothing,
                                 _srpLicenseType = Nothing,
                                 _srpEncrypted = Nothing,
                                 _srpKmsKeyId = Nothing, _srpRunOnce = Nothing}

-- | Frequency of creating replication jobs for the server.
srpFrequency :: Lens' ServerReplicationParameters (Maybe Int)
srpFrequency = lens _srpFrequency (\ s a -> s{_srpFrequency = a})

-- | Number of recent AMIs to keep when creating a replication job for this server.
srpNumberOfRecentAMIsToKeep :: Lens' ServerReplicationParameters (Maybe Int)
srpNumberOfRecentAMIsToKeep = lens _srpNumberOfRecentAMIsToKeep (\ s a -> s{_srpNumberOfRecentAMIsToKeep = a})

-- | Seed time for creating a replication job for the server.
srpSeedTime :: Lens' ServerReplicationParameters (Maybe UTCTime)
srpSeedTime = lens _srpSeedTime (\ s a -> s{_srpSeedTime = a}) . mapping _Time

-- | License type for creating a replication job for the server.
srpLicenseType :: Lens' ServerReplicationParameters (Maybe LicenseType)
srpLicenseType = lens _srpLicenseType (\ s a -> s{_srpLicenseType = a})

-- | When true, the replication job produces encrypted AMIs. See also @KmsKeyId@ below.
srpEncrypted :: Lens' ServerReplicationParameters (Maybe Bool)
srpEncrypted = lens _srpEncrypted (\ s a -> s{_srpEncrypted = a})

-- |  KMS key ID for replication jobs that produce encrypted AMIs. Can be any of the following:      * KMS key ID     * KMS key alias     * ARN referring to KMS key ID     * ARN referring to KMS key alias If encrypted is /true/ but a KMS key id is not specified, the customer's default KMS key for EBS is used. 
srpKmsKeyId :: Lens' ServerReplicationParameters (Maybe Text)
srpKmsKeyId = lens _srpKmsKeyId (\ s a -> s{_srpKmsKeyId = a})

-- | 
srpRunOnce :: Lens' ServerReplicationParameters (Maybe Bool)
srpRunOnce = lens _srpRunOnce (\ s a -> s{_srpRunOnce = a})

instance FromJSON ServerReplicationParameters where
        parseJSON
          = withObject "ServerReplicationParameters"
              (\ x ->
                 ServerReplicationParameters' <$>
                   (x .:? "frequency") <*>
                     (x .:? "numberOfRecentAmisToKeep")
                     <*> (x .:? "seedTime")
                     <*> (x .:? "licenseType")
                     <*> (x .:? "encrypted")
                     <*> (x .:? "kmsKeyId")
                     <*> (x .:? "runOnce"))

instance Hashable ServerReplicationParameters where

instance NFData ServerReplicationParameters where

instance ToJSON ServerReplicationParameters where
        toJSON ServerReplicationParameters'{..}
          = object
              (catMaybes
                 [("frequency" .=) <$> _srpFrequency,
                  ("numberOfRecentAmisToKeep" .=) <$>
                    _srpNumberOfRecentAMIsToKeep,
                  ("seedTime" .=) <$> _srpSeedTime,
                  ("licenseType" .=) <$> _srpLicenseType,
                  ("encrypted" .=) <$> _srpEncrypted,
                  ("kmsKeyId" .=) <$> _srpKmsKeyId,
                  ("runOnce" .=) <$> _srpRunOnce])
