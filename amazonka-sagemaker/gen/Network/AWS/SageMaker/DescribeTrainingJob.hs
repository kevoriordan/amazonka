{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeTrainingJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a training job.
--
--
module Network.AWS.SageMaker.DescribeTrainingJob
    (
    -- * Creating a Request
      describeTrainingJob
    , DescribeTrainingJob
    -- * Request Lenses
    , dtjTrainingJobName

    -- * Destructuring the Response
    , describeTrainingJobResponse
    , DescribeTrainingJobResponse
    -- * Response Lenses
    , desrsLabelingJobARN
    , desrsFailureReason
    , desrsSecondaryStatusTransitions
    , desrsTrainingEndTime
    , desrsBillableTimeInSeconds
    , desrsDebugHookConfig
    , desrsCheckpointConfig
    , desrsDebugRuleEvaluationStatuses
    , desrsEnableNetworkIsolation
    , desrsExperimentConfig
    , desrsLastModifiedTime
    , desrsDebugRuleConfigurations
    , desrsEnableManagedSpotTraining
    , desrsAutoMLJobARN
    , desrsHyperParameters
    , desrsInputDataConfig
    , desrsVPCConfig
    , desrsFinalMetricDataList
    , desrsOutputDataConfig
    , desrsTrainingStartTime
    , desrsTuningJobARN
    , desrsEnableInterContainerTrafficEncryption
    , desrsTensorBoardOutputConfig
    , desrsTrainingTimeInSeconds
    , desrsRoleARN
    , desrsResponseStatus
    , desrsTrainingJobName
    , desrsTrainingJobARN
    , desrsModelArtifacts
    , desrsTrainingJobStatus
    , desrsSecondaryStatus
    , desrsAlgorithmSpecification
    , desrsResourceConfig
    , desrsStoppingCondition
    , desrsCreationTime
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'describeTrainingJob' smart constructor.
newtype DescribeTrainingJob = DescribeTrainingJob'{_dtjTrainingJobName
                                                   :: Text}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'DescribeTrainingJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtjTrainingJobName' - The name of the training job.
describeTrainingJob
    :: Text -- ^ 'dtjTrainingJobName'
    -> DescribeTrainingJob
describeTrainingJob pTrainingJobName_
  = DescribeTrainingJob'{_dtjTrainingJobName =
                           pTrainingJobName_}

-- | The name of the training job.
dtjTrainingJobName :: Lens' DescribeTrainingJob Text
dtjTrainingJobName = lens _dtjTrainingJobName (\ s a -> s{_dtjTrainingJobName = a})

instance AWSRequest DescribeTrainingJob where
        type Rs DescribeTrainingJob =
             DescribeTrainingJobResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTrainingJobResponse' <$>
                   (x .?> "LabelingJobArn") <*> (x .?> "FailureReason")
                     <*> (x .?> "SecondaryStatusTransitions" .!@ mempty)
                     <*> (x .?> "TrainingEndTime")
                     <*> (x .?> "BillableTimeInSeconds")
                     <*> (x .?> "DebugHookConfig")
                     <*> (x .?> "CheckpointConfig")
                     <*> (x .?> "DebugRuleEvaluationStatuses" .!@ mempty)
                     <*> (x .?> "EnableNetworkIsolation")
                     <*> (x .?> "ExperimentConfig")
                     <*> (x .?> "LastModifiedTime")
                     <*> (x .?> "DebugRuleConfigurations" .!@ mempty)
                     <*> (x .?> "EnableManagedSpotTraining")
                     <*> (x .?> "AutoMLJobArn")
                     <*> (x .?> "HyperParameters" .!@ mempty)
                     <*> (x .?> "InputDataConfig")
                     <*> (x .?> "VpcConfig")
                     <*> (x .?> "FinalMetricDataList" .!@ mempty)
                     <*> (x .?> "OutputDataConfig")
                     <*> (x .?> "TrainingStartTime")
                     <*> (x .?> "TuningJobArn")
                     <*> (x .?> "EnableInterContainerTrafficEncryption")
                     <*> (x .?> "TensorBoardOutputConfig")
                     <*> (x .?> "TrainingTimeInSeconds")
                     <*> (x .?> "RoleArn")
                     <*> (pure (fromEnum s))
                     <*> (x .:> "TrainingJobName")
                     <*> (x .:> "TrainingJobArn")
                     <*> (x .:> "ModelArtifacts")
                     <*> (x .:> "TrainingJobStatus")
                     <*> (x .:> "SecondaryStatus")
                     <*> (x .:> "AlgorithmSpecification")
                     <*> (x .:> "ResourceConfig")
                     <*> (x .:> "StoppingCondition")
                     <*> (x .:> "CreationTime"))

instance Hashable DescribeTrainingJob where

instance NFData DescribeTrainingJob where

instance ToHeaders DescribeTrainingJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.DescribeTrainingJob" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeTrainingJob where
        toJSON DescribeTrainingJob'{..}
          = object
              (catMaybes
                 [Just ("TrainingJobName" .= _dtjTrainingJobName)])

instance ToPath DescribeTrainingJob where
        toPath = const "/"

instance ToQuery DescribeTrainingJob where
        toQuery = const mempty

-- | /See:/ 'describeTrainingJobResponse' smart constructor.
data DescribeTrainingJobResponse = DescribeTrainingJobResponse'{_desrsLabelingJobARN
                                                                ::
                                                                !(Maybe Text),
                                                                _desrsFailureReason
                                                                ::
                                                                !(Maybe Text),
                                                                _desrsSecondaryStatusTransitions
                                                                ::
                                                                !(Maybe
                                                                    [SecondaryStatusTransition]),
                                                                _desrsTrainingEndTime
                                                                ::
                                                                !(Maybe POSIX),
                                                                _desrsBillableTimeInSeconds
                                                                :: !(Maybe Nat),
                                                                _desrsDebugHookConfig
                                                                ::
                                                                !(Maybe
                                                                    DebugHookConfig),
                                                                _desrsCheckpointConfig
                                                                ::
                                                                !(Maybe
                                                                    CheckpointConfig),
                                                                _desrsDebugRuleEvaluationStatuses
                                                                ::
                                                                !(Maybe
                                                                    [DebugRuleEvaluationStatus]),
                                                                _desrsEnableNetworkIsolation
                                                                ::
                                                                !(Maybe Bool),
                                                                _desrsExperimentConfig
                                                                ::
                                                                !(Maybe
                                                                    ExperimentConfig),
                                                                _desrsLastModifiedTime
                                                                ::
                                                                !(Maybe POSIX),
                                                                _desrsDebugRuleConfigurations
                                                                ::
                                                                !(Maybe
                                                                    [DebugRuleConfiguration]),
                                                                _desrsEnableManagedSpotTraining
                                                                ::
                                                                !(Maybe Bool),
                                                                _desrsAutoMLJobARN
                                                                ::
                                                                !(Maybe Text),
                                                                _desrsHyperParameters
                                                                ::
                                                                !(Maybe
                                                                    (Map Text
                                                                       Text)),
                                                                _desrsInputDataConfig
                                                                ::
                                                                !(Maybe
                                                                    (List1
                                                                       Channel)),
                                                                _desrsVPCConfig
                                                                ::
                                                                !(Maybe
                                                                    VPCConfig),
                                                                _desrsFinalMetricDataList
                                                                ::
                                                                !(Maybe
                                                                    [MetricData]),
                                                                _desrsOutputDataConfig
                                                                ::
                                                                !(Maybe
                                                                    OutputDataConfig),
                                                                _desrsTrainingStartTime
                                                                ::
                                                                !(Maybe POSIX),
                                                                _desrsTuningJobARN
                                                                ::
                                                                !(Maybe Text),
                                                                _desrsEnableInterContainerTrafficEncryption
                                                                ::
                                                                !(Maybe Bool),
                                                                _desrsTensorBoardOutputConfig
                                                                ::
                                                                !(Maybe
                                                                    TensorBoardOutputConfig),
                                                                _desrsTrainingTimeInSeconds
                                                                :: !(Maybe Nat),
                                                                _desrsRoleARN ::
                                                                !(Maybe Text),
                                                                _desrsResponseStatus
                                                                :: !Int,
                                                                _desrsTrainingJobName
                                                                :: !Text,
                                                                _desrsTrainingJobARN
                                                                :: !Text,
                                                                _desrsModelArtifacts
                                                                ::
                                                                !ModelArtifacts,
                                                                _desrsTrainingJobStatus
                                                                ::
                                                                !TrainingJobStatus,
                                                                _desrsSecondaryStatus
                                                                ::
                                                                !SecondaryStatus,
                                                                _desrsAlgorithmSpecification
                                                                ::
                                                                !AlgorithmSpecification,
                                                                _desrsResourceConfig
                                                                ::
                                                                !ResourceConfig,
                                                                _desrsStoppingCondition
                                                                ::
                                                                !StoppingCondition,
                                                                _desrsCreationTime
                                                                :: !POSIX}
                                     deriving (Eq, Read, Show, Data, Typeable,
                                               Generic)

-- | Creates a value of 'DescribeTrainingJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desrsLabelingJobARN' - The Amazon Resource Name (ARN) of the Amazon SageMaker Ground Truth labeling job that created the transform or training job.
--
-- * 'desrsFailureReason' - If the training job failed, the reason it failed. 
--
-- * 'desrsSecondaryStatusTransitions' - A history of all of the secondary statuses that the training job has transitioned through.
--
-- * 'desrsTrainingEndTime' - Indicates the time when the training job ends on training instances. You are billed for the time interval between the value of @TrainingStartTime@ and this time. For successful jobs and stopped jobs, this is the time after model artifacts are uploaded. For failed jobs, this is the time when Amazon SageMaker detects a job failure.
--
-- * 'desrsBillableTimeInSeconds' - The billable time in seconds. You can calculate the savings from using managed spot training using the formula @(1 - BillableTimeInSeconds / TrainingTimeInSeconds) * 100@ . For example, if @BillableTimeInSeconds@ is 100 and @TrainingTimeInSeconds@ is 500, the savings is 80%.
--
-- * 'desrsDebugHookConfig' - Undocumented member.
--
-- * 'desrsCheckpointConfig' - Undocumented member.
--
-- * 'desrsDebugRuleEvaluationStatuses' - Status about the debug rule evaluation.
--
-- * 'desrsEnableNetworkIsolation' - If you want to allow inbound or outbound network calls, except for calls between peers within a training cluster for distributed training, choose @True@ . If you enable network isolation for training jobs that are configured to use a VPC, Amazon SageMaker downloads and uploads customer data and model artifacts through the specified VPC, but the training container does not have network access.
--
-- * 'desrsExperimentConfig' - Undocumented member.
--
-- * 'desrsLastModifiedTime' - A timestamp that indicates when the status of the training job was last modified.
--
-- * 'desrsDebugRuleConfigurations' - Configuration information for debugging rules.
--
-- * 'desrsEnableManagedSpotTraining' - A Boolean indicating whether managed spot training is enabled (@True@ ) or not (@False@ ).
--
-- * 'desrsAutoMLJobARN' - 
--
-- * 'desrsHyperParameters' - Algorithm-specific parameters. 
--
-- * 'desrsInputDataConfig' - An array of @Channel@ objects that describes each data input channel. 
--
-- * 'desrsVPCConfig' - A 'VpcConfig' object that specifies the VPC that this training job has access to. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
--
-- * 'desrsFinalMetricDataList' - A collection of @MetricData@ objects that specify the names, values, and dates and times that the training algorithm emitted to Amazon CloudWatch.
--
-- * 'desrsOutputDataConfig' - The S3 path where model artifacts that you configured when creating the job are stored. Amazon SageMaker creates subfolders for model artifacts. 
--
-- * 'desrsTrainingStartTime' - Indicates the time when the training job starts on training instances. You are billed for the time interval between this time and the value of @TrainingEndTime@ . The start time in CloudWatch Logs might be later than this time. The difference is due to the time it takes to download the training data and to the size of the training container.
--
-- * 'desrsTuningJobARN' - The Amazon Resource Name (ARN) of the associated hyperparameter tuning job if the training job was launched by a hyperparameter tuning job.
--
-- * 'desrsEnableInterContainerTrafficEncryption' - To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithms in distributed training.
--
-- * 'desrsTensorBoardOutputConfig' - Undocumented member.
--
-- * 'desrsTrainingTimeInSeconds' - The training time in seconds.
--
-- * 'desrsRoleARN' - The AWS Identity and Access Management (IAM) role configured for the training job. 
--
-- * 'desrsResponseStatus' - -- | The response status code.
--
-- * 'desrsTrainingJobName' - Name of the model training job. 
--
-- * 'desrsTrainingJobARN' - The Amazon Resource Name (ARN) of the training job.
--
-- * 'desrsModelArtifacts' - Information about the Amazon S3 location that is configured for storing model artifacts. 
--
-- * 'desrsTrainingJobStatus' - The status of the training job. Amazon SageMaker provides the following training job statuses:     * @InProgress@ - The training is in progress.     * @Completed@ - The training job has completed.     * @Failed@ - The training job has failed. To see the reason for the failure, see the @FailureReason@ field in the response to a @DescribeTrainingJobResponse@ call.     * @Stopping@ - The training job is stopping.     * @Stopped@ - The training job has stopped. For more detailed information, see @SecondaryStatus@ . 
--
-- * 'desrsSecondaryStatus' - Provides detailed information about the state of the training job. For detailed information on the secondary status of the training job, see @StatusMessage@ under 'SecondaryStatusTransition' . Amazon SageMaker provides primary statuses and secondary statuses that apply to each of them:     * InProgress    *     * @Starting@ - Starting the training job.     * @Downloading@ - An optional stage for algorithms that support @File@ training input mode. It indicates that data is being downloaded to the ML storage volumes.     * @Training@ - Training is in progress.     * @Interrupted@ - The job stopped because the managed spot training instances were interrupted.      * @Uploading@ - Training is complete and the model artifacts are being uploaded to the S3 location.     * Completed    *     * @Completed@ - The training job has completed.     * Failed    *     * @Failed@ - The training job has failed. The reason for the failure is returned in the @FailureReason@ field of @DescribeTrainingJobResponse@ .     * Stopped    *     * @MaxRuntimeExceeded@ - The job stopped because it exceeded the maximum allowed runtime.     * @MaxWaitTmeExceeded@ - The job stopped because it exceeded the maximum allowed wait time.     * @Stopped@ - The training job has stopped.     * Stopping    *     * @Stopping@ - Stopping the training job. /Important:/ Valid values for @SecondaryStatus@ are subject to change.  We no longer support the following secondary statuses:     * @LaunchingMLInstances@      * @PreparingTrainingStack@      * @DownloadingTrainingImage@ 
--
-- * 'desrsAlgorithmSpecification' - Information about the algorithm used for training, and algorithm metadata. 
--
-- * 'desrsResourceConfig' - Resources, including ML compute instances and ML storage volumes, that are configured for model training. 
--
-- * 'desrsStoppingCondition' - Specifies a limit to how long a model training job can run. It also specifies the maximum time to wait for a spot instance. When the job reaches the time limit, Amazon SageMaker ends the training job. Use this API to cap model training costs. To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal, which delays job termination for 120 seconds. Algorithms can use this 120-second window to save the model artifacts, so the results of training are not lost. 
--
-- * 'desrsCreationTime' - A timestamp that indicates when the training job was created.
describeTrainingJobResponse
    :: Int -- ^ 'desrsResponseStatus'
    -> Text -- ^ 'desrsTrainingJobName'
    -> Text -- ^ 'desrsTrainingJobARN'
    -> ModelArtifacts -- ^ 'desrsModelArtifacts'
    -> TrainingJobStatus -- ^ 'desrsTrainingJobStatus'
    -> SecondaryStatus -- ^ 'desrsSecondaryStatus'
    -> AlgorithmSpecification -- ^ 'desrsAlgorithmSpecification'
    -> ResourceConfig -- ^ 'desrsResourceConfig'
    -> StoppingCondition -- ^ 'desrsStoppingCondition'
    -> UTCTime -- ^ 'desrsCreationTime'
    -> DescribeTrainingJobResponse
describeTrainingJobResponse pResponseStatus_
  pTrainingJobName_ pTrainingJobARN_ pModelArtifacts_
  pTrainingJobStatus_ pSecondaryStatus_
  pAlgorithmSpecification_ pResourceConfig_
  pStoppingCondition_ pCreationTime_
  = DescribeTrainingJobResponse'{_desrsLabelingJobARN =
                                   Nothing,
                                 _desrsFailureReason = Nothing,
                                 _desrsSecondaryStatusTransitions = Nothing,
                                 _desrsTrainingEndTime = Nothing,
                                 _desrsBillableTimeInSeconds = Nothing,
                                 _desrsDebugHookConfig = Nothing,
                                 _desrsCheckpointConfig = Nothing,
                                 _desrsDebugRuleEvaluationStatuses = Nothing,
                                 _desrsEnableNetworkIsolation = Nothing,
                                 _desrsExperimentConfig = Nothing,
                                 _desrsLastModifiedTime = Nothing,
                                 _desrsDebugRuleConfigurations = Nothing,
                                 _desrsEnableManagedSpotTraining = Nothing,
                                 _desrsAutoMLJobARN = Nothing,
                                 _desrsHyperParameters = Nothing,
                                 _desrsInputDataConfig = Nothing,
                                 _desrsVPCConfig = Nothing,
                                 _desrsFinalMetricDataList = Nothing,
                                 _desrsOutputDataConfig = Nothing,
                                 _desrsTrainingStartTime = Nothing,
                                 _desrsTuningJobARN = Nothing,
                                 _desrsEnableInterContainerTrafficEncryption =
                                   Nothing,
                                 _desrsTensorBoardOutputConfig = Nothing,
                                 _desrsTrainingTimeInSeconds = Nothing,
                                 _desrsRoleARN = Nothing,
                                 _desrsResponseStatus = pResponseStatus_,
                                 _desrsTrainingJobName = pTrainingJobName_,
                                 _desrsTrainingJobARN = pTrainingJobARN_,
                                 _desrsModelArtifacts = pModelArtifacts_,
                                 _desrsTrainingJobStatus = pTrainingJobStatus_,
                                 _desrsSecondaryStatus = pSecondaryStatus_,
                                 _desrsAlgorithmSpecification =
                                   pAlgorithmSpecification_,
                                 _desrsResourceConfig = pResourceConfig_,
                                 _desrsStoppingCondition = pStoppingCondition_,
                                 _desrsCreationTime = _Time # pCreationTime_}

-- | The Amazon Resource Name (ARN) of the Amazon SageMaker Ground Truth labeling job that created the transform or training job.
desrsLabelingJobARN :: Lens' DescribeTrainingJobResponse (Maybe Text)
desrsLabelingJobARN = lens _desrsLabelingJobARN (\ s a -> s{_desrsLabelingJobARN = a})

-- | If the training job failed, the reason it failed. 
desrsFailureReason :: Lens' DescribeTrainingJobResponse (Maybe Text)
desrsFailureReason = lens _desrsFailureReason (\ s a -> s{_desrsFailureReason = a})

-- | A history of all of the secondary statuses that the training job has transitioned through.
desrsSecondaryStatusTransitions :: Lens' DescribeTrainingJobResponse [SecondaryStatusTransition]
desrsSecondaryStatusTransitions = lens _desrsSecondaryStatusTransitions (\ s a -> s{_desrsSecondaryStatusTransitions = a}) . _Default . _Coerce

-- | Indicates the time when the training job ends on training instances. You are billed for the time interval between the value of @TrainingStartTime@ and this time. For successful jobs and stopped jobs, this is the time after model artifacts are uploaded. For failed jobs, this is the time when Amazon SageMaker detects a job failure.
desrsTrainingEndTime :: Lens' DescribeTrainingJobResponse (Maybe UTCTime)
desrsTrainingEndTime = lens _desrsTrainingEndTime (\ s a -> s{_desrsTrainingEndTime = a}) . mapping _Time

-- | The billable time in seconds. You can calculate the savings from using managed spot training using the formula @(1 - BillableTimeInSeconds / TrainingTimeInSeconds) * 100@ . For example, if @BillableTimeInSeconds@ is 100 and @TrainingTimeInSeconds@ is 500, the savings is 80%.
desrsBillableTimeInSeconds :: Lens' DescribeTrainingJobResponse (Maybe Natural)
desrsBillableTimeInSeconds = lens _desrsBillableTimeInSeconds (\ s a -> s{_desrsBillableTimeInSeconds = a}) . mapping _Nat

-- | Undocumented member.
desrsDebugHookConfig :: Lens' DescribeTrainingJobResponse (Maybe DebugHookConfig)
desrsDebugHookConfig = lens _desrsDebugHookConfig (\ s a -> s{_desrsDebugHookConfig = a})

-- | Undocumented member.
desrsCheckpointConfig :: Lens' DescribeTrainingJobResponse (Maybe CheckpointConfig)
desrsCheckpointConfig = lens _desrsCheckpointConfig (\ s a -> s{_desrsCheckpointConfig = a})

-- | Status about the debug rule evaluation.
desrsDebugRuleEvaluationStatuses :: Lens' DescribeTrainingJobResponse [DebugRuleEvaluationStatus]
desrsDebugRuleEvaluationStatuses = lens _desrsDebugRuleEvaluationStatuses (\ s a -> s{_desrsDebugRuleEvaluationStatuses = a}) . _Default . _Coerce

-- | If you want to allow inbound or outbound network calls, except for calls between peers within a training cluster for distributed training, choose @True@ . If you enable network isolation for training jobs that are configured to use a VPC, Amazon SageMaker downloads and uploads customer data and model artifacts through the specified VPC, but the training container does not have network access.
desrsEnableNetworkIsolation :: Lens' DescribeTrainingJobResponse (Maybe Bool)
desrsEnableNetworkIsolation = lens _desrsEnableNetworkIsolation (\ s a -> s{_desrsEnableNetworkIsolation = a})

-- | Undocumented member.
desrsExperimentConfig :: Lens' DescribeTrainingJobResponse (Maybe ExperimentConfig)
desrsExperimentConfig = lens _desrsExperimentConfig (\ s a -> s{_desrsExperimentConfig = a})

-- | A timestamp that indicates when the status of the training job was last modified.
desrsLastModifiedTime :: Lens' DescribeTrainingJobResponse (Maybe UTCTime)
desrsLastModifiedTime = lens _desrsLastModifiedTime (\ s a -> s{_desrsLastModifiedTime = a}) . mapping _Time

-- | Configuration information for debugging rules.
desrsDebugRuleConfigurations :: Lens' DescribeTrainingJobResponse [DebugRuleConfiguration]
desrsDebugRuleConfigurations = lens _desrsDebugRuleConfigurations (\ s a -> s{_desrsDebugRuleConfigurations = a}) . _Default . _Coerce

-- | A Boolean indicating whether managed spot training is enabled (@True@ ) or not (@False@ ).
desrsEnableManagedSpotTraining :: Lens' DescribeTrainingJobResponse (Maybe Bool)
desrsEnableManagedSpotTraining = lens _desrsEnableManagedSpotTraining (\ s a -> s{_desrsEnableManagedSpotTraining = a})

-- | 
desrsAutoMLJobARN :: Lens' DescribeTrainingJobResponse (Maybe Text)
desrsAutoMLJobARN = lens _desrsAutoMLJobARN (\ s a -> s{_desrsAutoMLJobARN = a})

-- | Algorithm-specific parameters. 
desrsHyperParameters :: Lens' DescribeTrainingJobResponse (HashMap Text Text)
desrsHyperParameters = lens _desrsHyperParameters (\ s a -> s{_desrsHyperParameters = a}) . _Default . _Map

-- | An array of @Channel@ objects that describes each data input channel. 
desrsInputDataConfig :: Lens' DescribeTrainingJobResponse (Maybe (NonEmpty Channel))
desrsInputDataConfig = lens _desrsInputDataConfig (\ s a -> s{_desrsInputDataConfig = a}) . mapping _List1

-- | A 'VpcConfig' object that specifies the VPC that this training job has access to. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
desrsVPCConfig :: Lens' DescribeTrainingJobResponse (Maybe VPCConfig)
desrsVPCConfig = lens _desrsVPCConfig (\ s a -> s{_desrsVPCConfig = a})

-- | A collection of @MetricData@ objects that specify the names, values, and dates and times that the training algorithm emitted to Amazon CloudWatch.
desrsFinalMetricDataList :: Lens' DescribeTrainingJobResponse [MetricData]
desrsFinalMetricDataList = lens _desrsFinalMetricDataList (\ s a -> s{_desrsFinalMetricDataList = a}) . _Default . _Coerce

-- | The S3 path where model artifacts that you configured when creating the job are stored. Amazon SageMaker creates subfolders for model artifacts. 
desrsOutputDataConfig :: Lens' DescribeTrainingJobResponse (Maybe OutputDataConfig)
desrsOutputDataConfig = lens _desrsOutputDataConfig (\ s a -> s{_desrsOutputDataConfig = a})

-- | Indicates the time when the training job starts on training instances. You are billed for the time interval between this time and the value of @TrainingEndTime@ . The start time in CloudWatch Logs might be later than this time. The difference is due to the time it takes to download the training data and to the size of the training container.
desrsTrainingStartTime :: Lens' DescribeTrainingJobResponse (Maybe UTCTime)
desrsTrainingStartTime = lens _desrsTrainingStartTime (\ s a -> s{_desrsTrainingStartTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the associated hyperparameter tuning job if the training job was launched by a hyperparameter tuning job.
desrsTuningJobARN :: Lens' DescribeTrainingJobResponse (Maybe Text)
desrsTuningJobARN = lens _desrsTuningJobARN (\ s a -> s{_desrsTuningJobARN = a})

-- | To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithms in distributed training.
desrsEnableInterContainerTrafficEncryption :: Lens' DescribeTrainingJobResponse (Maybe Bool)
desrsEnableInterContainerTrafficEncryption = lens _desrsEnableInterContainerTrafficEncryption (\ s a -> s{_desrsEnableInterContainerTrafficEncryption = a})

-- | Undocumented member.
desrsTensorBoardOutputConfig :: Lens' DescribeTrainingJobResponse (Maybe TensorBoardOutputConfig)
desrsTensorBoardOutputConfig = lens _desrsTensorBoardOutputConfig (\ s a -> s{_desrsTensorBoardOutputConfig = a})

-- | The training time in seconds.
desrsTrainingTimeInSeconds :: Lens' DescribeTrainingJobResponse (Maybe Natural)
desrsTrainingTimeInSeconds = lens _desrsTrainingTimeInSeconds (\ s a -> s{_desrsTrainingTimeInSeconds = a}) . mapping _Nat

-- | The AWS Identity and Access Management (IAM) role configured for the training job. 
desrsRoleARN :: Lens' DescribeTrainingJobResponse (Maybe Text)
desrsRoleARN = lens _desrsRoleARN (\ s a -> s{_desrsRoleARN = a})

-- | -- | The response status code.
desrsResponseStatus :: Lens' DescribeTrainingJobResponse Int
desrsResponseStatus = lens _desrsResponseStatus (\ s a -> s{_desrsResponseStatus = a})

-- | Name of the model training job. 
desrsTrainingJobName :: Lens' DescribeTrainingJobResponse Text
desrsTrainingJobName = lens _desrsTrainingJobName (\ s a -> s{_desrsTrainingJobName = a})

-- | The Amazon Resource Name (ARN) of the training job.
desrsTrainingJobARN :: Lens' DescribeTrainingJobResponse Text
desrsTrainingJobARN = lens _desrsTrainingJobARN (\ s a -> s{_desrsTrainingJobARN = a})

-- | Information about the Amazon S3 location that is configured for storing model artifacts. 
desrsModelArtifacts :: Lens' DescribeTrainingJobResponse ModelArtifacts
desrsModelArtifacts = lens _desrsModelArtifacts (\ s a -> s{_desrsModelArtifacts = a})

-- | The status of the training job. Amazon SageMaker provides the following training job statuses:     * @InProgress@ - The training is in progress.     * @Completed@ - The training job has completed.     * @Failed@ - The training job has failed. To see the reason for the failure, see the @FailureReason@ field in the response to a @DescribeTrainingJobResponse@ call.     * @Stopping@ - The training job is stopping.     * @Stopped@ - The training job has stopped. For more detailed information, see @SecondaryStatus@ . 
desrsTrainingJobStatus :: Lens' DescribeTrainingJobResponse TrainingJobStatus
desrsTrainingJobStatus = lens _desrsTrainingJobStatus (\ s a -> s{_desrsTrainingJobStatus = a})

-- | Provides detailed information about the state of the training job. For detailed information on the secondary status of the training job, see @StatusMessage@ under 'SecondaryStatusTransition' . Amazon SageMaker provides primary statuses and secondary statuses that apply to each of them:     * InProgress    *     * @Starting@ - Starting the training job.     * @Downloading@ - An optional stage for algorithms that support @File@ training input mode. It indicates that data is being downloaded to the ML storage volumes.     * @Training@ - Training is in progress.     * @Interrupted@ - The job stopped because the managed spot training instances were interrupted.      * @Uploading@ - Training is complete and the model artifacts are being uploaded to the S3 location.     * Completed    *     * @Completed@ - The training job has completed.     * Failed    *     * @Failed@ - The training job has failed. The reason for the failure is returned in the @FailureReason@ field of @DescribeTrainingJobResponse@ .     * Stopped    *     * @MaxRuntimeExceeded@ - The job stopped because it exceeded the maximum allowed runtime.     * @MaxWaitTmeExceeded@ - The job stopped because it exceeded the maximum allowed wait time.     * @Stopped@ - The training job has stopped.     * Stopping    *     * @Stopping@ - Stopping the training job. /Important:/ Valid values for @SecondaryStatus@ are subject to change.  We no longer support the following secondary statuses:     * @LaunchingMLInstances@      * @PreparingTrainingStack@      * @DownloadingTrainingImage@ 
desrsSecondaryStatus :: Lens' DescribeTrainingJobResponse SecondaryStatus
desrsSecondaryStatus = lens _desrsSecondaryStatus (\ s a -> s{_desrsSecondaryStatus = a})

-- | Information about the algorithm used for training, and algorithm metadata. 
desrsAlgorithmSpecification :: Lens' DescribeTrainingJobResponse AlgorithmSpecification
desrsAlgorithmSpecification = lens _desrsAlgorithmSpecification (\ s a -> s{_desrsAlgorithmSpecification = a})

-- | Resources, including ML compute instances and ML storage volumes, that are configured for model training. 
desrsResourceConfig :: Lens' DescribeTrainingJobResponse ResourceConfig
desrsResourceConfig = lens _desrsResourceConfig (\ s a -> s{_desrsResourceConfig = a})

-- | Specifies a limit to how long a model training job can run. It also specifies the maximum time to wait for a spot instance. When the job reaches the time limit, Amazon SageMaker ends the training job. Use this API to cap model training costs. To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal, which delays job termination for 120 seconds. Algorithms can use this 120-second window to save the model artifacts, so the results of training are not lost. 
desrsStoppingCondition :: Lens' DescribeTrainingJobResponse StoppingCondition
desrsStoppingCondition = lens _desrsStoppingCondition (\ s a -> s{_desrsStoppingCondition = a})

-- | A timestamp that indicates when the training job was created.
desrsCreationTime :: Lens' DescribeTrainingJobResponse UTCTime
desrsCreationTime = lens _desrsCreationTime (\ s a -> s{_desrsCreationTime = a}) . _Time

instance NFData DescribeTrainingJobResponse where
