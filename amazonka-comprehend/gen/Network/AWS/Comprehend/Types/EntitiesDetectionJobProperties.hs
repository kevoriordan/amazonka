{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntitiesDetectionJobProperties
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.EntitiesDetectionJobProperties where

import Network.AWS.Comprehend.Types.InputDataConfig
import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Comprehend.Types.LanguageCode
import Network.AWS.Comprehend.Types.OutputDataConfig
import Network.AWS.Comprehend.Types.VPCConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about an entities detection job.
--
--
--
-- /See:/ 'entitiesDetectionJobProperties' smart constructor.
data EntitiesDetectionJobProperties = EntitiesDetectionJobProperties'{_edjpLanguageCode
                                                                      ::
                                                                      !(Maybe
                                                                          LanguageCode),
                                                                      _edjpJobId
                                                                      ::
                                                                      !(Maybe
                                                                          Text),
                                                                      _edjpEntityRecognizerARN
                                                                      ::
                                                                      !(Maybe
                                                                          Text),
                                                                      _edjpJobName
                                                                      ::
                                                                      !(Maybe
                                                                          Text),
                                                                      _edjpInputDataConfig
                                                                      ::
                                                                      !(Maybe
                                                                          InputDataConfig),
                                                                      _edjpVPCConfig
                                                                      ::
                                                                      !(Maybe
                                                                          VPCConfig),
                                                                      _edjpVolumeKMSKeyId
                                                                      ::
                                                                      !(Maybe
                                                                          Text),
                                                                      _edjpEndTime
                                                                      ::
                                                                      !(Maybe
                                                                          POSIX),
                                                                      _edjpOutputDataConfig
                                                                      ::
                                                                      !(Maybe
                                                                          OutputDataConfig),
                                                                      _edjpDataAccessRoleARN
                                                                      ::
                                                                      !(Maybe
                                                                          Text),
                                                                      _edjpJobStatus
                                                                      ::
                                                                      !(Maybe
                                                                          JobStatus),
                                                                      _edjpMessage
                                                                      ::
                                                                      !(Maybe
                                                                          Text),
                                                                      _edjpSubmitTime
                                                                      ::
                                                                      !(Maybe
                                                                          POSIX)}
                                        deriving (Eq, Read, Show, Data,
                                                  Typeable, Generic)

-- | Creates a value of 'EntitiesDetectionJobProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edjpLanguageCode' - The language code of the input documents.
--
-- * 'edjpJobId' - The identifier assigned to the entities detection job.
--
-- * 'edjpEntityRecognizerARN' - The Amazon Resource Name (ARN) that identifies the entity recognizer.
--
-- * 'edjpJobName' - The name that you assigned the entities detection job.
--
-- * 'edjpInputDataConfig' - The input data configuration that you supplied when you created the entities detection job.
--
-- * 'edjpVPCConfig' - Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your entity detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> . 
--
-- * 'edjpVolumeKMSKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@ 
--
-- * 'edjpEndTime' - The time that the entities detection job completed
--
-- * 'edjpOutputDataConfig' - The output data configuration that you supplied when you created the entities detection job. 
--
-- * 'edjpDataAccessRoleARN' - The Amazon Resource Name (ARN) that gives Amazon Comprehend read access to your input data.
--
-- * 'edjpJobStatus' - The current status of the entities detection job. If the status is @FAILED@ , the @Message@ field shows the reason for the failure.
--
-- * 'edjpMessage' - A description of the status of a job.
--
-- * 'edjpSubmitTime' - The time that the entities detection job was submitted for processing.
entitiesDetectionJobProperties
    :: EntitiesDetectionJobProperties
entitiesDetectionJobProperties
  = EntitiesDetectionJobProperties'{_edjpLanguageCode =
                                      Nothing,
                                    _edjpJobId = Nothing,
                                    _edjpEntityRecognizerARN = Nothing,
                                    _edjpJobName = Nothing,
                                    _edjpInputDataConfig = Nothing,
                                    _edjpVPCConfig = Nothing,
                                    _edjpVolumeKMSKeyId = Nothing,
                                    _edjpEndTime = Nothing,
                                    _edjpOutputDataConfig = Nothing,
                                    _edjpDataAccessRoleARN = Nothing,
                                    _edjpJobStatus = Nothing,
                                    _edjpMessage = Nothing,
                                    _edjpSubmitTime = Nothing}

-- | The language code of the input documents.
edjpLanguageCode :: Lens' EntitiesDetectionJobProperties (Maybe LanguageCode)
edjpLanguageCode = lens _edjpLanguageCode (\ s a -> s{_edjpLanguageCode = a})

-- | The identifier assigned to the entities detection job.
edjpJobId :: Lens' EntitiesDetectionJobProperties (Maybe Text)
edjpJobId = lens _edjpJobId (\ s a -> s{_edjpJobId = a})

-- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
edjpEntityRecognizerARN :: Lens' EntitiesDetectionJobProperties (Maybe Text)
edjpEntityRecognizerARN = lens _edjpEntityRecognizerARN (\ s a -> s{_edjpEntityRecognizerARN = a})

-- | The name that you assigned the entities detection job.
edjpJobName :: Lens' EntitiesDetectionJobProperties (Maybe Text)
edjpJobName = lens _edjpJobName (\ s a -> s{_edjpJobName = a})

-- | The input data configuration that you supplied when you created the entities detection job.
edjpInputDataConfig :: Lens' EntitiesDetectionJobProperties (Maybe InputDataConfig)
edjpInputDataConfig = lens _edjpInputDataConfig (\ s a -> s{_edjpInputDataConfig = a})

-- | Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your entity detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> . 
edjpVPCConfig :: Lens' EntitiesDetectionJobProperties (Maybe VPCConfig)
edjpVPCConfig = lens _edjpVPCConfig (\ s a -> s{_edjpVPCConfig = a})

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@ 
edjpVolumeKMSKeyId :: Lens' EntitiesDetectionJobProperties (Maybe Text)
edjpVolumeKMSKeyId = lens _edjpVolumeKMSKeyId (\ s a -> s{_edjpVolumeKMSKeyId = a})

-- | The time that the entities detection job completed
edjpEndTime :: Lens' EntitiesDetectionJobProperties (Maybe UTCTime)
edjpEndTime = lens _edjpEndTime (\ s a -> s{_edjpEndTime = a}) . mapping _Time

-- | The output data configuration that you supplied when you created the entities detection job. 
edjpOutputDataConfig :: Lens' EntitiesDetectionJobProperties (Maybe OutputDataConfig)
edjpOutputDataConfig = lens _edjpOutputDataConfig (\ s a -> s{_edjpOutputDataConfig = a})

-- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access to your input data.
edjpDataAccessRoleARN :: Lens' EntitiesDetectionJobProperties (Maybe Text)
edjpDataAccessRoleARN = lens _edjpDataAccessRoleARN (\ s a -> s{_edjpDataAccessRoleARN = a})

-- | The current status of the entities detection job. If the status is @FAILED@ , the @Message@ field shows the reason for the failure.
edjpJobStatus :: Lens' EntitiesDetectionJobProperties (Maybe JobStatus)
edjpJobStatus = lens _edjpJobStatus (\ s a -> s{_edjpJobStatus = a})

-- | A description of the status of a job.
edjpMessage :: Lens' EntitiesDetectionJobProperties (Maybe Text)
edjpMessage = lens _edjpMessage (\ s a -> s{_edjpMessage = a})

-- | The time that the entities detection job was submitted for processing.
edjpSubmitTime :: Lens' EntitiesDetectionJobProperties (Maybe UTCTime)
edjpSubmitTime = lens _edjpSubmitTime (\ s a -> s{_edjpSubmitTime = a}) . mapping _Time

instance FromJSON EntitiesDetectionJobProperties
         where
        parseJSON
          = withObject "EntitiesDetectionJobProperties"
              (\ x ->
                 EntitiesDetectionJobProperties' <$>
                   (x .:? "LanguageCode") <*> (x .:? "JobId") <*>
                     (x .:? "EntityRecognizerArn")
                     <*> (x .:? "JobName")
                     <*> (x .:? "InputDataConfig")
                     <*> (x .:? "VpcConfig")
                     <*> (x .:? "VolumeKmsKeyId")
                     <*> (x .:? "EndTime")
                     <*> (x .:? "OutputDataConfig")
                     <*> (x .:? "DataAccessRoleArn")
                     <*> (x .:? "JobStatus")
                     <*> (x .:? "Message")
                     <*> (x .:? "SubmitTime"))

instance Hashable EntitiesDetectionJobProperties
         where

instance NFData EntitiesDetectionJobProperties where
