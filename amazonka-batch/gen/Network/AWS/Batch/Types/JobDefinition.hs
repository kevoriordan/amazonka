{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.JobDefinition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Batch.Types.JobDefinition where

import Network.AWS.Batch.Types.ContainerProperties
import Network.AWS.Batch.Types.JobTimeout
import Network.AWS.Batch.Types.NodeProperties
import Network.AWS.Batch.Types.RetryStrategy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing an AWS Batch job definition.
--
--
--
-- /See:/ 'jobDefinition' smart constructor.
data JobDefinition = JobDefinition'{_jbdfntnStatus ::
                                    !(Maybe Text),
                                    _jbdfntnRetryStrategy ::
                                    !(Maybe RetryStrategy),
                                    _jbdfntnParameters ::
                                    !(Maybe (Map Text Text)),
                                    _jbdfntnTimeout :: !(Maybe JobTimeout),
                                    _jbdfntnContainerProperties ::
                                    !(Maybe ContainerProperties),
                                    _jbdfntnNodeProperties ::
                                    !(Maybe NodeProperties),
                                    _jbdfntnJobDefinitionName :: !Text,
                                    _jbdfntnJobDefinitionARN :: !Text,
                                    _jbdfntnRevision :: !Int,
                                    _jbdfntnType :: !Text}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JobDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jbdfntnStatus' - The status of the job definition.
--
-- * 'jbdfntnRetryStrategy' - The retry strategy to use for failed jobs that are submitted with this job definition.
--
-- * 'jbdfntnParameters' - Default parameters or parameter substitution placeholders that are set in the job definition. Parameters are specified as a key-value pair mapping. Parameters in a @SubmitJob@ request override any corresponding parameter defaults from the job definition. For more information about specifying parameters, see <https://docs.aws.amazon.com/batch/latest/userguide/job_definition_parameters.html Job Definition Parameters> in the /AWS Batch User Guide/ .
--
-- * 'jbdfntnTimeout' - The timeout configuration for jobs that are submitted with this job definition. You can specify a timeout duration after which AWS Batch terminates your jobs if they have not finished.
--
-- * 'jbdfntnContainerProperties' - An object with various properties specific to container-based jobs.
--
-- * 'jbdfntnNodeProperties' - An object with various properties specific to multi-node parallel jobs.
--
-- * 'jbdfntnJobDefinitionName' - The name of the job definition.
--
-- * 'jbdfntnJobDefinitionARN' - The Amazon Resource Name (ARN) for the job definition.
--
-- * 'jbdfntnRevision' - The revision of the job definition.
--
-- * 'jbdfntnType' - The type of job definition.
jobDefinition
    :: Text -- ^ 'jbdfntnJobDefinitionName'
    -> Text -- ^ 'jbdfntnJobDefinitionARN'
    -> Int -- ^ 'jbdfntnRevision'
    -> Text -- ^ 'jbdfntnType'
    -> JobDefinition
jobDefinition pJobDefinitionName_ pJobDefinitionARN_
  pRevision_ pType_
  = JobDefinition'{_jbdfntnStatus = Nothing,
                   _jbdfntnRetryStrategy = Nothing,
                   _jbdfntnParameters = Nothing,
                   _jbdfntnTimeout = Nothing,
                   _jbdfntnContainerProperties = Nothing,
                   _jbdfntnNodeProperties = Nothing,
                   _jbdfntnJobDefinitionName = pJobDefinitionName_,
                   _jbdfntnJobDefinitionARN = pJobDefinitionARN_,
                   _jbdfntnRevision = pRevision_, _jbdfntnType = pType_}

-- | The status of the job definition.
jbdfntnStatus :: Lens' JobDefinition (Maybe Text)
jbdfntnStatus = lens _jbdfntnStatus (\ s a -> s{_jbdfntnStatus = a})

-- | The retry strategy to use for failed jobs that are submitted with this job definition.
jbdfntnRetryStrategy :: Lens' JobDefinition (Maybe RetryStrategy)
jbdfntnRetryStrategy = lens _jbdfntnRetryStrategy (\ s a -> s{_jbdfntnRetryStrategy = a})

-- | Default parameters or parameter substitution placeholders that are set in the job definition. Parameters are specified as a key-value pair mapping. Parameters in a @SubmitJob@ request override any corresponding parameter defaults from the job definition. For more information about specifying parameters, see <https://docs.aws.amazon.com/batch/latest/userguide/job_definition_parameters.html Job Definition Parameters> in the /AWS Batch User Guide/ .
jbdfntnParameters :: Lens' JobDefinition (HashMap Text Text)
jbdfntnParameters = lens _jbdfntnParameters (\ s a -> s{_jbdfntnParameters = a}) . _Default . _Map

-- | The timeout configuration for jobs that are submitted with this job definition. You can specify a timeout duration after which AWS Batch terminates your jobs if they have not finished.
jbdfntnTimeout :: Lens' JobDefinition (Maybe JobTimeout)
jbdfntnTimeout = lens _jbdfntnTimeout (\ s a -> s{_jbdfntnTimeout = a})

-- | An object with various properties specific to container-based jobs.
jbdfntnContainerProperties :: Lens' JobDefinition (Maybe ContainerProperties)
jbdfntnContainerProperties = lens _jbdfntnContainerProperties (\ s a -> s{_jbdfntnContainerProperties = a})

-- | An object with various properties specific to multi-node parallel jobs.
jbdfntnNodeProperties :: Lens' JobDefinition (Maybe NodeProperties)
jbdfntnNodeProperties = lens _jbdfntnNodeProperties (\ s a -> s{_jbdfntnNodeProperties = a})

-- | The name of the job definition.
jbdfntnJobDefinitionName :: Lens' JobDefinition Text
jbdfntnJobDefinitionName = lens _jbdfntnJobDefinitionName (\ s a -> s{_jbdfntnJobDefinitionName = a})

-- | The Amazon Resource Name (ARN) for the job definition.
jbdfntnJobDefinitionARN :: Lens' JobDefinition Text
jbdfntnJobDefinitionARN = lens _jbdfntnJobDefinitionARN (\ s a -> s{_jbdfntnJobDefinitionARN = a})

-- | The revision of the job definition.
jbdfntnRevision :: Lens' JobDefinition Int
jbdfntnRevision = lens _jbdfntnRevision (\ s a -> s{_jbdfntnRevision = a})

-- | The type of job definition.
jbdfntnType :: Lens' JobDefinition Text
jbdfntnType = lens _jbdfntnType (\ s a -> s{_jbdfntnType = a})

instance FromJSON JobDefinition where
        parseJSON
          = withObject "JobDefinition"
              (\ x ->
                 JobDefinition' <$>
                   (x .:? "status") <*> (x .:? "retryStrategy") <*>
                     (x .:? "parameters" .!= mempty)
                     <*> (x .:? "timeout")
                     <*> (x .:? "containerProperties")
                     <*> (x .:? "nodeProperties")
                     <*> (x .: "jobDefinitionName")
                     <*> (x .: "jobDefinitionArn")
                     <*> (x .: "revision")
                     <*> (x .: "type"))

instance Hashable JobDefinition where

instance NFData JobDefinition where
