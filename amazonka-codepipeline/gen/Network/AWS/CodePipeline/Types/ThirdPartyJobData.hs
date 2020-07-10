{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ThirdPartyJobData
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.ThirdPartyJobData where

import Network.AWS.CodePipeline.Types.AWSSessionCredentials
import Network.AWS.CodePipeline.Types.ActionConfiguration
import Network.AWS.CodePipeline.Types.ActionTypeId
import Network.AWS.CodePipeline.Types.Artifact
import Network.AWS.CodePipeline.Types.EncryptionKey
import Network.AWS.CodePipeline.Types.PipelineContext
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents information about the job data for a partner action.
--
--
--
-- /See:/ 'thirdPartyJobData' smart constructor.
data ThirdPartyJobData = ThirdPartyJobData'{_tpjdContinuationToken
                                            :: !(Maybe Text),
                                            _tpjdOutputArtifacts ::
                                            !(Maybe [Artifact]),
                                            _tpjdArtifactCredentials ::
                                            !(Maybe
                                                (Sensitive
                                                   AWSSessionCredentials)),
                                            _tpjdPipelineContext ::
                                            !(Maybe PipelineContext),
                                            _tpjdEncryptionKey ::
                                            !(Maybe EncryptionKey),
                                            _tpjdActionTypeId ::
                                            !(Maybe ActionTypeId),
                                            _tpjdInputArtifacts ::
                                            !(Maybe [Artifact]),
                                            _tpjdActionConfiguration ::
                                            !(Maybe ActionConfiguration)}
                           deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ThirdPartyJobData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tpjdContinuationToken' - A system-generated token, such as a AWS CodeDeploy deployment ID, that a job requires to continue the job asynchronously.
--
-- * 'tpjdOutputArtifacts' - The name of the artifact that is the result of the action, if any. This name might be system-generated, such as "MyBuiltApp", or it might be defined by the user when the action is created.
--
-- * 'tpjdArtifactCredentials' - Represents an AWS session credentials object. These credentials are temporary credentials that are issued by AWS Secure Token Service (STS). They can be used to access input and output artifacts in the S3 bucket used to store artifact for the pipeline in AWS CodePipeline. 
--
-- * 'tpjdPipelineContext' - Represents information about a pipeline to a job worker.
--
-- * 'tpjdEncryptionKey' - The encryption key used to encrypt and decrypt data in the artifact store for the pipeline, such as an AWS Key Management Service (AWS KMS) key. This is optional and might not be present.
--
-- * 'tpjdActionTypeId' - Represents information about an action type.
--
-- * 'tpjdInputArtifacts' - The name of the artifact that is worked on by the action, if any. This name might be system-generated, such as "MyApp", or it might be defined by the user when the action is created. The input artifact name must match the name of an output artifact generated by an action in an earlier action or stage of the pipeline.
--
-- * 'tpjdActionConfiguration' - Represents information about an action configuration.
thirdPartyJobData
    :: ThirdPartyJobData
thirdPartyJobData
  = ThirdPartyJobData'{_tpjdContinuationToken =
                         Nothing,
                       _tpjdOutputArtifacts = Nothing,
                       _tpjdArtifactCredentials = Nothing,
                       _tpjdPipelineContext = Nothing,
                       _tpjdEncryptionKey = Nothing,
                       _tpjdActionTypeId = Nothing,
                       _tpjdInputArtifacts = Nothing,
                       _tpjdActionConfiguration = Nothing}

-- | A system-generated token, such as a AWS CodeDeploy deployment ID, that a job requires to continue the job asynchronously.
tpjdContinuationToken :: Lens' ThirdPartyJobData (Maybe Text)
tpjdContinuationToken = lens _tpjdContinuationToken (\ s a -> s{_tpjdContinuationToken = a})

-- | The name of the artifact that is the result of the action, if any. This name might be system-generated, such as "MyBuiltApp", or it might be defined by the user when the action is created.
tpjdOutputArtifacts :: Lens' ThirdPartyJobData [Artifact]
tpjdOutputArtifacts = lens _tpjdOutputArtifacts (\ s a -> s{_tpjdOutputArtifacts = a}) . _Default . _Coerce

-- | Represents an AWS session credentials object. These credentials are temporary credentials that are issued by AWS Secure Token Service (STS). They can be used to access input and output artifacts in the S3 bucket used to store artifact for the pipeline in AWS CodePipeline. 
tpjdArtifactCredentials :: Lens' ThirdPartyJobData (Maybe AWSSessionCredentials)
tpjdArtifactCredentials = lens _tpjdArtifactCredentials (\ s a -> s{_tpjdArtifactCredentials = a}) . mapping _Sensitive

-- | Represents information about a pipeline to a job worker.
tpjdPipelineContext :: Lens' ThirdPartyJobData (Maybe PipelineContext)
tpjdPipelineContext = lens _tpjdPipelineContext (\ s a -> s{_tpjdPipelineContext = a})

-- | The encryption key used to encrypt and decrypt data in the artifact store for the pipeline, such as an AWS Key Management Service (AWS KMS) key. This is optional and might not be present.
tpjdEncryptionKey :: Lens' ThirdPartyJobData (Maybe EncryptionKey)
tpjdEncryptionKey = lens _tpjdEncryptionKey (\ s a -> s{_tpjdEncryptionKey = a})

-- | Represents information about an action type.
tpjdActionTypeId :: Lens' ThirdPartyJobData (Maybe ActionTypeId)
tpjdActionTypeId = lens _tpjdActionTypeId (\ s a -> s{_tpjdActionTypeId = a})

-- | The name of the artifact that is worked on by the action, if any. This name might be system-generated, such as "MyApp", or it might be defined by the user when the action is created. The input artifact name must match the name of an output artifact generated by an action in an earlier action or stage of the pipeline.
tpjdInputArtifacts :: Lens' ThirdPartyJobData [Artifact]
tpjdInputArtifacts = lens _tpjdInputArtifacts (\ s a -> s{_tpjdInputArtifacts = a}) . _Default . _Coerce

-- | Represents information about an action configuration.
tpjdActionConfiguration :: Lens' ThirdPartyJobData (Maybe ActionConfiguration)
tpjdActionConfiguration = lens _tpjdActionConfiguration (\ s a -> s{_tpjdActionConfiguration = a})

instance FromJSON ThirdPartyJobData where
        parseJSON
          = withObject "ThirdPartyJobData"
              (\ x ->
                 ThirdPartyJobData' <$>
                   (x .:? "continuationToken") <*>
                     (x .:? "outputArtifacts" .!= mempty)
                     <*> (x .:? "artifactCredentials")
                     <*> (x .:? "pipelineContext")
                     <*> (x .:? "encryptionKey")
                     <*> (x .:? "actionTypeId")
                     <*> (x .:? "inputArtifacts" .!= mempty)
                     <*> (x .:? "actionConfiguration"))

instance Hashable ThirdPartyJobData where

instance NFData ThirdPartyJobData where
