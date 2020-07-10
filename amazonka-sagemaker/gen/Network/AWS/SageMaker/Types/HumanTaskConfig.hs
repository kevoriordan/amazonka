{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HumanTaskConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.HumanTaskConfig where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.AnnotationConsolidationConfig
import Network.AWS.SageMaker.Types.PublicWorkforceTaskPrice
import Network.AWS.SageMaker.Types.UiConfig

-- | Information required for human workers to complete a labeling task.
--
--
--
-- /See:/ 'humanTaskConfig' smart constructor.
data HumanTaskConfig = HumanTaskConfig'{_htcTaskKeywords
                                        :: !(Maybe (List1 Text)),
                                        _htcPublicWorkforceTaskPrice ::
                                        !(Maybe PublicWorkforceTaskPrice),
                                        _htcTaskAvailabilityLifetimeInSeconds ::
                                        !(Maybe Nat),
                                        _htcMaxConcurrentTaskCount ::
                                        !(Maybe Nat),
                                        _htcWorkteamARN :: !Text,
                                        _htcUiConfig :: !UiConfig,
                                        _htcPreHumanTaskLambdaARN :: !Text,
                                        _htcTaskTitle :: !Text,
                                        _htcTaskDescription :: !Text,
                                        _htcNumberOfHumanWorkersPerDataObject ::
                                        !Nat,
                                        _htcTaskTimeLimitInSeconds :: !Nat,
                                        _htcAnnotationConsolidationConfig ::
                                        !AnnotationConsolidationConfig}
                         deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HumanTaskConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'htcTaskKeywords' - Keywords used to describe the task so that workers on Amazon Mechanical Turk can discover the task.
--
-- * 'htcPublicWorkforceTaskPrice' - The price that you pay for each task performed by an Amazon Mechanical Turk worker.
--
-- * 'htcTaskAvailabilityLifetimeInSeconds' - The length of time that a task remains available for labeling by human workers. __If you choose the Amazon Mechanical Turk workforce, the maximum is 12 hours (43200)__ . The default value is 864000 seconds (10 days). For private and vendor workforces, the maximum is as listed.
--
-- * 'htcMaxConcurrentTaskCount' - Defines the maximum number of data objects that can be labeled by human workers at the same time. Also referred to as batch size. Each object may have more than one worker at one time. The default value is 1000 objects.
--
-- * 'htcWorkteamARN' - The Amazon Resource Name (ARN) of the work team assigned to complete the tasks.
--
-- * 'htcUiConfig' - Information about the user interface that workers use to complete the labeling task.
--
-- * 'htcPreHumanTaskLambdaARN' - The Amazon Resource Name (ARN) of a Lambda function that is run before a data object is sent to a human worker. Use this function to provide input to a custom labeling job. For the built-in bounding box, image classification, semantic segmentation, and text classification task types, Amazon SageMaker Ground Truth provides the following Lambda functions: __US East (Northern Virginia) (us-east-1):__      * @arn:aws:lambda:us-east-1:432418664414:function:PRE-BoundingBox@      * @arn:aws:lambda:us-east-1:432418664414:function:PRE-ImageMultiClass@      * @arn:aws:lambda:us-east-1:432418664414:function:PRE-ImageMultiClassMultiLabel@      * @arn:aws:lambda:us-east-1:432418664414:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:us-east-1:432418664414:function:PRE-TextMultiClass@      * @arn:aws:lambda:us-east-1:432418664414:function:PRE-TextMultiClassMultiLabel@      * @arn:aws:lambda:us-east-1:432418664414:function:PRE-NamedEntityRecognition@      * @arn:aws:lambda:us-east-1:432418664414:function:PRE-VerificationBoundingBox@      * @arn:aws:lambda:us-east-1:432418664414:function:PRE-VerificationSemanticSegmentation@      * @arn:aws:lambda:us-east-1:432418664414:function:PRE-AdjustmentBoundingBox@      * @arn:aws:lambda:us-east-1:432418664414:function:PRE-AdjustmentSemanticSegmentation@  __US East (Ohio) (us-east-2):__      * @arn:aws:lambda:us-east-2:266458841044:function:PRE-BoundingBox@      * @arn:aws:lambda:us-east-2:266458841044:function:PRE-ImageMultiClass@      * @arn:aws:lambda:us-east-2:266458841044:function:PRE-ImageMultiClassMultiLabel@      * @arn:aws:lambda:us-east-2:266458841044:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:us-east-2:266458841044:function:PRE-TextMultiClass@      * @arn:aws:lambda:us-east-2:266458841044:function:PRE-TextMultiClassMultiLabel@      * @arn:aws:lambda:us-east-2:266458841044:function:PRE-NamedEntityRecognition@      * @arn:aws:lambda:us-east-2:266458841044:function:PRE-VerificationBoundingBox@      * @arn:aws:lambda:us-east-2:266458841044:function:PRE-VerificationSemanticSegmentation@      * @arn:aws:lambda:us-east-2:266458841044:function:PRE-AdjustmentBoundingBox@      * @arn:aws:lambda:us-east-2:266458841044:function:PRE-AdjustmentSemanticSegmentation@  __US West (Oregon) (us-west-2):__      * @arn:aws:lambda:us-west-2:081040173940:function:PRE-BoundingBox@      * @arn:aws:lambda:us-west-2:081040173940:function:PRE-ImageMultiClass@      * @arn:aws:lambda:us-west-2:081040173940:function:PRE-ImageMultiClassMultiLabel@      * @arn:aws:lambda:us-west-2:081040173940:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:us-west-2:081040173940:function:PRE-TextMultiClass@      * @arn:aws:lambda:us-west-2:081040173940:function:PRE-TextMultiClassMultiLabel@      * @arn:aws:lambda:us-west-2:081040173940:function:PRE-NamedEntityRecognition@      * @arn:aws:lambda:us-west-2:081040173940:function:PRE-VerificationBoundingBox@      * @arn:aws:lambda:us-west-2:081040173940:function:PRE-VerificationSemanticSegmentation@      * @arn:aws:lambda:us-west-2:081040173940:function:PRE-AdjustmentBoundingBox@      * @arn:aws:lambda:us-west-2:081040173940:function:PRE-AdjustmentSemanticSegmentation@  __Canada (Central) (ca-central-1):__      * @arn:aws:lambda:ca-central-1:918755190332:function:PRE-BoundingBox@      * @arn:aws:lambda:ca-central-1:918755190332:function:PRE-ImageMultiClass@      * @arn:aws:lambda:ca-central-1:918755190332:function:PRE-ImageMultiClassMultiLabel@      * @arn:aws:lambda:ca-central-1:918755190332:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:ca-central-1:918755190332:function:PRE-TextMultiClass@      * @arn:aws:lambda:ca-central-1:918755190332:function:PRE-TextMultiClassMultiLabel@      * @arn:aws:lambda:ca-central-1:918755190332:function:PRE-NamedEntityRecognition@      * @arn:aws:lambda:ca-central-1:918755190332:function:PRE-VerificationBoundingBox@      * @arn:aws:lambda:ca-central-1:918755190332:function:PRE-VerificationSemanticSegmentation@      * @arn:aws:lambda:ca-central-1:918755190332:function:PRE-AdjustmentBoundingBox@      * @arn:aws:lambda:ca-central-1:918755190332:function:PRE-AdjustmentSemanticSegmentation@  __EU (Ireland) (eu-west-1):__      * @arn:aws:lambda:eu-west-1:568282634449:function:PRE-BoundingBox@      * @arn:aws:lambda:eu-west-1:568282634449:function:PRE-ImageMultiClass@      * @arn:aws:lambda:eu-west-1:568282634449:function:PRE-ImageMultiClassMultiLabel@      * @arn:aws:lambda:eu-west-1:568282634449:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:eu-west-1:568282634449:function:PRE-TextMultiClass@      * @arn:aws:lambda:eu-west-1:568282634449:function:PRE-TextMultiClassMultiLabel@      * @arn:aws:lambda:eu-west-1:568282634449:function:PRE-NamedEntityRecognition@      * @arn:aws:lambda:eu-west-1:568282634449:function:PRE-VerificationBoundingBox@      * @arn:aws:lambda:eu-west-1:568282634449:function:PRE-VerificationSemanticSegmentation@      * @arn:aws:lambda:eu-west-1:568282634449:function:PRE-AdjustmentBoundingBox@      * @arn:aws:lambda:eu-west-1:568282634449:function:PRE-AdjustmentSemanticSegmentation@  __EU (London) (eu-west-2):__      * @arn:aws:lambda:eu-west-2:487402164563:function:PRE-BoundingBox@      * @arn:aws:lambda:eu-west-2:487402164563:function:PRE-ImageMultiClass@      * @arn:aws:lambda:eu-west-2:487402164563:function:PRE-ImageMultiClassMultiLabel@      * @arn:aws:lambda:eu-west-2:487402164563:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:eu-west-2:487402164563:function:PRE-TextMultiClass@      * @arn:aws:lambda:eu-west-2:487402164563:function:PRE-TextMultiClassMultiLabel@      * @arn:aws:lambda:eu-west-2:487402164563:function:PRE-NamedEntityRecognition@      * @arn:aws:lambda:eu-west-2:487402164563:function:PRE-VerificationBoundingBox@      * @arn:aws:lambda:eu-west-2:487402164563:function:PRE-VerificationSemanticSegmentation@      * @arn:aws:lambda:eu-west-2:487402164563:function:PRE-AdjustmentBoundingBox@      * @arn:aws:lambda:eu-west-2:487402164563:function:PRE-AdjustmentSemanticSegmentation@  __EU Frankfurt (eu-central-1):__      * @arn:aws:lambda:eu-central-1:203001061592:function:PRE-BoundingBox@      * @arn:aws:lambda:eu-central-1:203001061592:function:PRE-ImageMultiClass@      * @arn:aws:lambda:eu-central-1:203001061592:function:PRE-ImageMultiClassMultiLabel@      * @arn:aws:lambda:eu-central-1:203001061592:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:eu-central-1:203001061592:function:PRE-TextMultiClass@      * @arn:aws:lambda:eu-central-1:203001061592:function:PRE-TextMultiClassMultiLabel@      * @arn:aws:lambda:eu-central-1:203001061592:function:PRE-NamedEntityRecognition@      * @arn:aws:lambda:eu-central-1:203001061592:function:PRE-VerificationBoundingBox@      * @arn:aws:lambda:eu-central-1:203001061592:function:PRE-VerificationSemanticSegmentation@      * @arn:aws:lambda:eu-central-1:203001061592:function:PRE-AdjustmentBoundingBox@      * @arn:aws:lambda:eu-central-1:203001061592:function:PRE-AdjustmentSemanticSegmentation@  __Asia Pacific (Tokyo) (ap-northeast-1):__      * @arn:aws:lambda:ap-northeast-1:477331159723:function:PRE-BoundingBox@      * @arn:aws:lambda:ap-northeast-1:477331159723:function:PRE-ImageMultiClass@      * @arn:aws:lambda:ap-northeast-1:477331159723:function:PRE-ImageMultiClassMultiLabel@      * @arn:aws:lambda:ap-northeast-1:477331159723:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:ap-northeast-1:477331159723:function:PRE-TextMultiClass@      * @arn:aws:lambda:ap-northeast-1:477331159723:function:PRE-TextMultiClassMultiLabel@      * @arn:aws:lambda:ap-northeast-1:477331159723:function:PRE-NamedEntityRecognition@      * @arn:aws:lambda:ap-northeast-1:477331159723:function:PRE-VerificationBoundingBox@      * @arn:aws:lambda:ap-northeast-1:477331159723:function:PRE-VerificationSemanticSegmentation@      * @arn:aws:lambda:ap-northeast-1:477331159723:function:PRE-AdjustmentBoundingBox@      * @arn:aws:lambda:ap-northeast-1:477331159723:function:PRE-AdjustmentSemanticSegmentation@  __Asia Pacific (Seoul) (ap-northeast-2):__      * @arn:aws:lambda:ap-northeast-2:845288260483:function:PRE-BoundingBox@      * @arn:aws:lambda:ap-northeast-2:845288260483:function:PRE-ImageMultiClass@      * @arn:aws:lambda:ap-northeast-2:845288260483:function:PRE-ImageMultiClassMultiLabel@      * @arn:aws:lambda:ap-northeast-2:845288260483:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:ap-northeast-2:845288260483:function:PRE-TextMultiClass@      * @arn:aws:lambda:ap-northeast-2:845288260483:function:PRE-TextMultiClassMultiLabel@      * @arn:aws:lambda:ap-northeast-2:845288260483:function:PRE-NamedEntityRecognition@      * @arn:aws:lambda:ap-northeast-2:845288260483:function:PRE-VerificationBoundingBox@      * @arn:aws:lambda:ap-northeast-2:845288260483:function:PRE-VerificationSemanticSegmentation@      * @arn:aws:lambda:ap-northeast-2:845288260483:function:PRE-AdjustmentBoundingBox@      * @arn:aws:lambda:ap-northeast-2:845288260483:function:PRE-AdjustmentSemanticSegmentation@  __Asia Pacific (Mumbai) (ap-south-1):__      * @arn:aws:lambda:ap-south-1:565803892007:function:PRE-BoundingBox@      * @arn:aws:lambda:ap-south-1:565803892007:function:PRE-ImageMultiClass@      * @arn:aws:lambda:ap-south-1:565803892007:function:PRE-ImageMultiClassMultiLabel@      * @arn:aws:lambda:ap-south-1:565803892007:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:ap-south-1:565803892007:function:PRE-TextMultiClass@      * @arn:aws:lambda:ap-south-1:565803892007:function:PRE-TextMultiClassMultiLabel@      * @arn:aws:lambda:ap-south-1:565803892007:function:PRE-NamedEntityRecognition@      * @arn:aws:lambda:ap-south-1:565803892007:function:PRE-VerificationBoundingBox@      * @arn:aws:lambda:ap-south-1:565803892007:function:PRE-VerificationSemanticSegmentation@      * @arn:aws:lambda:ap-south-1:565803892007:function:PRE-AdjustmentBoundingBox@      * @arn:aws:lambda:ap-south-1:565803892007:function:PRE-AdjustmentSemanticSegmentation@  __Asia Pacific (Singapore) (ap-southeast-1):__      * @arn:aws:lambda:ap-southeast-1:377565633583:function:PRE-BoundingBox@      * @arn:aws:lambda:ap-southeast-1:377565633583:function:PRE-ImageMultiClass@      * @arn:aws:lambda:ap-southeast-1:377565633583:function:PRE-ImageMultiClassMultiLabel@      * @arn:aws:lambda:ap-southeast-1:377565633583:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:ap-southeast-1:377565633583:function:PRE-TextMultiClass@      * @arn:aws:lambda:ap-southeast-1:377565633583:function:PRE-TextMultiClassMultiLabel@      * @arn:aws:lambda:ap-southeast-1:377565633583:function:PRE-NamedEntityRecognition@      * @arn:aws:lambda:ap-southeast-1:377565633583:function:PRE-VerificationBoundingBox@      * @arn:aws:lambda:ap-southeast-1:377565633583:function:PRE-VerificationSemanticSegmentation@      * @arn:aws:lambda:ap-southeast-1:377565633583:function:PRE-AdjustmentBoundingBox@      * @arn:aws:lambda:ap-southeast-1:377565633583:function:PRE-AdjustmentSemanticSegmentation@  __Asia Pacific (Sydney) (ap-southeast-2):__      * @arn:aws:lambda:ap-southeast-2:454466003867:function:PRE-BoundingBox@      * @arn:aws:lambda:ap-southeast-2:454466003867:function:PRE-ImageMultiClass@      * @arn:aws:lambda:ap-southeast-2:454466003867:function:PRE-ImageMultiClassMultiLabel@      * @arn:aws:lambda:ap-southeast-2:454466003867:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:ap-southeast-2:454466003867:function:PRE-TextMultiClass@      * @arn:aws:lambda:ap-southeast-2:454466003867:function:PRE-TextMultiClassMultiLabel@      * @arn:aws:lambda:ap-southeast-2:454466003867:function:PRE-NamedEntityRecognition@      * @arn:aws:lambda:ap-southeast-2:454466003867:function:PRE-VerificationBoundingBox@      * @arn:aws:lambda:ap-southeast-2:454466003867:function:PRE-VerificationSemanticSegmentation@      * @arn:aws:lambda:ap-southeast-2:454466003867:function:PRE-AdjustmentBoundingBox@      * @arn:aws:lambda:ap-southeast-2:454466003867:function:PRE-AdjustmentSemanticSegmentation@ 
--
-- * 'htcTaskTitle' - A title for the task for your human workers.
--
-- * 'htcTaskDescription' - A description of the task for your human workers.
--
-- * 'htcNumberOfHumanWorkersPerDataObject' - The number of human workers that will label an object. 
--
-- * 'htcTaskTimeLimitInSeconds' - The amount of time that a worker has to complete a task.
--
-- * 'htcAnnotationConsolidationConfig' - Configures how labels are consolidated across human workers.
humanTaskConfig
    :: Text -- ^ 'htcWorkteamARN'
    -> UiConfig -- ^ 'htcUiConfig'
    -> Text -- ^ 'htcPreHumanTaskLambdaARN'
    -> Text -- ^ 'htcTaskTitle'
    -> Text -- ^ 'htcTaskDescription'
    -> Natural -- ^ 'htcNumberOfHumanWorkersPerDataObject'
    -> Natural -- ^ 'htcTaskTimeLimitInSeconds'
    -> AnnotationConsolidationConfig -- ^ 'htcAnnotationConsolidationConfig'
    -> HumanTaskConfig
humanTaskConfig pWorkteamARN_ pUiConfig_
  pPreHumanTaskLambdaARN_ pTaskTitle_ pTaskDescription_
  pNumberOfHumanWorkersPerDataObject_
  pTaskTimeLimitInSeconds_
  pAnnotationConsolidationConfig_
  = HumanTaskConfig'{_htcTaskKeywords = Nothing,
                     _htcPublicWorkforceTaskPrice = Nothing,
                     _htcTaskAvailabilityLifetimeInSeconds = Nothing,
                     _htcMaxConcurrentTaskCount = Nothing,
                     _htcWorkteamARN = pWorkteamARN_,
                     _htcUiConfig = pUiConfig_,
                     _htcPreHumanTaskLambdaARN = pPreHumanTaskLambdaARN_,
                     _htcTaskTitle = pTaskTitle_,
                     _htcTaskDescription = pTaskDescription_,
                     _htcNumberOfHumanWorkersPerDataObject =
                       _Nat # pNumberOfHumanWorkersPerDataObject_,
                     _htcTaskTimeLimitInSeconds =
                       _Nat # pTaskTimeLimitInSeconds_,
                     _htcAnnotationConsolidationConfig =
                       pAnnotationConsolidationConfig_}

-- | Keywords used to describe the task so that workers on Amazon Mechanical Turk can discover the task.
htcTaskKeywords :: Lens' HumanTaskConfig (Maybe (NonEmpty Text))
htcTaskKeywords = lens _htcTaskKeywords (\ s a -> s{_htcTaskKeywords = a}) . mapping _List1

-- | The price that you pay for each task performed by an Amazon Mechanical Turk worker.
htcPublicWorkforceTaskPrice :: Lens' HumanTaskConfig (Maybe PublicWorkforceTaskPrice)
htcPublicWorkforceTaskPrice = lens _htcPublicWorkforceTaskPrice (\ s a -> s{_htcPublicWorkforceTaskPrice = a})

-- | The length of time that a task remains available for labeling by human workers. __If you choose the Amazon Mechanical Turk workforce, the maximum is 12 hours (43200)__ . The default value is 864000 seconds (10 days). For private and vendor workforces, the maximum is as listed.
htcTaskAvailabilityLifetimeInSeconds :: Lens' HumanTaskConfig (Maybe Natural)
htcTaskAvailabilityLifetimeInSeconds = lens _htcTaskAvailabilityLifetimeInSeconds (\ s a -> s{_htcTaskAvailabilityLifetimeInSeconds = a}) . mapping _Nat

-- | Defines the maximum number of data objects that can be labeled by human workers at the same time. Also referred to as batch size. Each object may have more than one worker at one time. The default value is 1000 objects.
htcMaxConcurrentTaskCount :: Lens' HumanTaskConfig (Maybe Natural)
htcMaxConcurrentTaskCount = lens _htcMaxConcurrentTaskCount (\ s a -> s{_htcMaxConcurrentTaskCount = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the work team assigned to complete the tasks.
htcWorkteamARN :: Lens' HumanTaskConfig Text
htcWorkteamARN = lens _htcWorkteamARN (\ s a -> s{_htcWorkteamARN = a})

-- | Information about the user interface that workers use to complete the labeling task.
htcUiConfig :: Lens' HumanTaskConfig UiConfig
htcUiConfig = lens _htcUiConfig (\ s a -> s{_htcUiConfig = a})

-- | The Amazon Resource Name (ARN) of a Lambda function that is run before a data object is sent to a human worker. Use this function to provide input to a custom labeling job. For the built-in bounding box, image classification, semantic segmentation, and text classification task types, Amazon SageMaker Ground Truth provides the following Lambda functions: __US East (Northern Virginia) (us-east-1):__      * @arn:aws:lambda:us-east-1:432418664414:function:PRE-BoundingBox@      * @arn:aws:lambda:us-east-1:432418664414:function:PRE-ImageMultiClass@      * @arn:aws:lambda:us-east-1:432418664414:function:PRE-ImageMultiClassMultiLabel@      * @arn:aws:lambda:us-east-1:432418664414:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:us-east-1:432418664414:function:PRE-TextMultiClass@      * @arn:aws:lambda:us-east-1:432418664414:function:PRE-TextMultiClassMultiLabel@      * @arn:aws:lambda:us-east-1:432418664414:function:PRE-NamedEntityRecognition@      * @arn:aws:lambda:us-east-1:432418664414:function:PRE-VerificationBoundingBox@      * @arn:aws:lambda:us-east-1:432418664414:function:PRE-VerificationSemanticSegmentation@      * @arn:aws:lambda:us-east-1:432418664414:function:PRE-AdjustmentBoundingBox@      * @arn:aws:lambda:us-east-1:432418664414:function:PRE-AdjustmentSemanticSegmentation@  __US East (Ohio) (us-east-2):__      * @arn:aws:lambda:us-east-2:266458841044:function:PRE-BoundingBox@      * @arn:aws:lambda:us-east-2:266458841044:function:PRE-ImageMultiClass@      * @arn:aws:lambda:us-east-2:266458841044:function:PRE-ImageMultiClassMultiLabel@      * @arn:aws:lambda:us-east-2:266458841044:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:us-east-2:266458841044:function:PRE-TextMultiClass@      * @arn:aws:lambda:us-east-2:266458841044:function:PRE-TextMultiClassMultiLabel@      * @arn:aws:lambda:us-east-2:266458841044:function:PRE-NamedEntityRecognition@      * @arn:aws:lambda:us-east-2:266458841044:function:PRE-VerificationBoundingBox@      * @arn:aws:lambda:us-east-2:266458841044:function:PRE-VerificationSemanticSegmentation@      * @arn:aws:lambda:us-east-2:266458841044:function:PRE-AdjustmentBoundingBox@      * @arn:aws:lambda:us-east-2:266458841044:function:PRE-AdjustmentSemanticSegmentation@  __US West (Oregon) (us-west-2):__      * @arn:aws:lambda:us-west-2:081040173940:function:PRE-BoundingBox@      * @arn:aws:lambda:us-west-2:081040173940:function:PRE-ImageMultiClass@      * @arn:aws:lambda:us-west-2:081040173940:function:PRE-ImageMultiClassMultiLabel@      * @arn:aws:lambda:us-west-2:081040173940:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:us-west-2:081040173940:function:PRE-TextMultiClass@      * @arn:aws:lambda:us-west-2:081040173940:function:PRE-TextMultiClassMultiLabel@      * @arn:aws:lambda:us-west-2:081040173940:function:PRE-NamedEntityRecognition@      * @arn:aws:lambda:us-west-2:081040173940:function:PRE-VerificationBoundingBox@      * @arn:aws:lambda:us-west-2:081040173940:function:PRE-VerificationSemanticSegmentation@      * @arn:aws:lambda:us-west-2:081040173940:function:PRE-AdjustmentBoundingBox@      * @arn:aws:lambda:us-west-2:081040173940:function:PRE-AdjustmentSemanticSegmentation@  __Canada (Central) (ca-central-1):__      * @arn:aws:lambda:ca-central-1:918755190332:function:PRE-BoundingBox@      * @arn:aws:lambda:ca-central-1:918755190332:function:PRE-ImageMultiClass@      * @arn:aws:lambda:ca-central-1:918755190332:function:PRE-ImageMultiClassMultiLabel@      * @arn:aws:lambda:ca-central-1:918755190332:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:ca-central-1:918755190332:function:PRE-TextMultiClass@      * @arn:aws:lambda:ca-central-1:918755190332:function:PRE-TextMultiClassMultiLabel@      * @arn:aws:lambda:ca-central-1:918755190332:function:PRE-NamedEntityRecognition@      * @arn:aws:lambda:ca-central-1:918755190332:function:PRE-VerificationBoundingBox@      * @arn:aws:lambda:ca-central-1:918755190332:function:PRE-VerificationSemanticSegmentation@      * @arn:aws:lambda:ca-central-1:918755190332:function:PRE-AdjustmentBoundingBox@      * @arn:aws:lambda:ca-central-1:918755190332:function:PRE-AdjustmentSemanticSegmentation@  __EU (Ireland) (eu-west-1):__      * @arn:aws:lambda:eu-west-1:568282634449:function:PRE-BoundingBox@      * @arn:aws:lambda:eu-west-1:568282634449:function:PRE-ImageMultiClass@      * @arn:aws:lambda:eu-west-1:568282634449:function:PRE-ImageMultiClassMultiLabel@      * @arn:aws:lambda:eu-west-1:568282634449:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:eu-west-1:568282634449:function:PRE-TextMultiClass@      * @arn:aws:lambda:eu-west-1:568282634449:function:PRE-TextMultiClassMultiLabel@      * @arn:aws:lambda:eu-west-1:568282634449:function:PRE-NamedEntityRecognition@      * @arn:aws:lambda:eu-west-1:568282634449:function:PRE-VerificationBoundingBox@      * @arn:aws:lambda:eu-west-1:568282634449:function:PRE-VerificationSemanticSegmentation@      * @arn:aws:lambda:eu-west-1:568282634449:function:PRE-AdjustmentBoundingBox@      * @arn:aws:lambda:eu-west-1:568282634449:function:PRE-AdjustmentSemanticSegmentation@  __EU (London) (eu-west-2):__      * @arn:aws:lambda:eu-west-2:487402164563:function:PRE-BoundingBox@      * @arn:aws:lambda:eu-west-2:487402164563:function:PRE-ImageMultiClass@      * @arn:aws:lambda:eu-west-2:487402164563:function:PRE-ImageMultiClassMultiLabel@      * @arn:aws:lambda:eu-west-2:487402164563:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:eu-west-2:487402164563:function:PRE-TextMultiClass@      * @arn:aws:lambda:eu-west-2:487402164563:function:PRE-TextMultiClassMultiLabel@      * @arn:aws:lambda:eu-west-2:487402164563:function:PRE-NamedEntityRecognition@      * @arn:aws:lambda:eu-west-2:487402164563:function:PRE-VerificationBoundingBox@      * @arn:aws:lambda:eu-west-2:487402164563:function:PRE-VerificationSemanticSegmentation@      * @arn:aws:lambda:eu-west-2:487402164563:function:PRE-AdjustmentBoundingBox@      * @arn:aws:lambda:eu-west-2:487402164563:function:PRE-AdjustmentSemanticSegmentation@  __EU Frankfurt (eu-central-1):__      * @arn:aws:lambda:eu-central-1:203001061592:function:PRE-BoundingBox@      * @arn:aws:lambda:eu-central-1:203001061592:function:PRE-ImageMultiClass@      * @arn:aws:lambda:eu-central-1:203001061592:function:PRE-ImageMultiClassMultiLabel@      * @arn:aws:lambda:eu-central-1:203001061592:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:eu-central-1:203001061592:function:PRE-TextMultiClass@      * @arn:aws:lambda:eu-central-1:203001061592:function:PRE-TextMultiClassMultiLabel@      * @arn:aws:lambda:eu-central-1:203001061592:function:PRE-NamedEntityRecognition@      * @arn:aws:lambda:eu-central-1:203001061592:function:PRE-VerificationBoundingBox@      * @arn:aws:lambda:eu-central-1:203001061592:function:PRE-VerificationSemanticSegmentation@      * @arn:aws:lambda:eu-central-1:203001061592:function:PRE-AdjustmentBoundingBox@      * @arn:aws:lambda:eu-central-1:203001061592:function:PRE-AdjustmentSemanticSegmentation@  __Asia Pacific (Tokyo) (ap-northeast-1):__      * @arn:aws:lambda:ap-northeast-1:477331159723:function:PRE-BoundingBox@      * @arn:aws:lambda:ap-northeast-1:477331159723:function:PRE-ImageMultiClass@      * @arn:aws:lambda:ap-northeast-1:477331159723:function:PRE-ImageMultiClassMultiLabel@      * @arn:aws:lambda:ap-northeast-1:477331159723:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:ap-northeast-1:477331159723:function:PRE-TextMultiClass@      * @arn:aws:lambda:ap-northeast-1:477331159723:function:PRE-TextMultiClassMultiLabel@      * @arn:aws:lambda:ap-northeast-1:477331159723:function:PRE-NamedEntityRecognition@      * @arn:aws:lambda:ap-northeast-1:477331159723:function:PRE-VerificationBoundingBox@      * @arn:aws:lambda:ap-northeast-1:477331159723:function:PRE-VerificationSemanticSegmentation@      * @arn:aws:lambda:ap-northeast-1:477331159723:function:PRE-AdjustmentBoundingBox@      * @arn:aws:lambda:ap-northeast-1:477331159723:function:PRE-AdjustmentSemanticSegmentation@  __Asia Pacific (Seoul) (ap-northeast-2):__      * @arn:aws:lambda:ap-northeast-2:845288260483:function:PRE-BoundingBox@      * @arn:aws:lambda:ap-northeast-2:845288260483:function:PRE-ImageMultiClass@      * @arn:aws:lambda:ap-northeast-2:845288260483:function:PRE-ImageMultiClassMultiLabel@      * @arn:aws:lambda:ap-northeast-2:845288260483:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:ap-northeast-2:845288260483:function:PRE-TextMultiClass@      * @arn:aws:lambda:ap-northeast-2:845288260483:function:PRE-TextMultiClassMultiLabel@      * @arn:aws:lambda:ap-northeast-2:845288260483:function:PRE-NamedEntityRecognition@      * @arn:aws:lambda:ap-northeast-2:845288260483:function:PRE-VerificationBoundingBox@      * @arn:aws:lambda:ap-northeast-2:845288260483:function:PRE-VerificationSemanticSegmentation@      * @arn:aws:lambda:ap-northeast-2:845288260483:function:PRE-AdjustmentBoundingBox@      * @arn:aws:lambda:ap-northeast-2:845288260483:function:PRE-AdjustmentSemanticSegmentation@  __Asia Pacific (Mumbai) (ap-south-1):__      * @arn:aws:lambda:ap-south-1:565803892007:function:PRE-BoundingBox@      * @arn:aws:lambda:ap-south-1:565803892007:function:PRE-ImageMultiClass@      * @arn:aws:lambda:ap-south-1:565803892007:function:PRE-ImageMultiClassMultiLabel@      * @arn:aws:lambda:ap-south-1:565803892007:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:ap-south-1:565803892007:function:PRE-TextMultiClass@      * @arn:aws:lambda:ap-south-1:565803892007:function:PRE-TextMultiClassMultiLabel@      * @arn:aws:lambda:ap-south-1:565803892007:function:PRE-NamedEntityRecognition@      * @arn:aws:lambda:ap-south-1:565803892007:function:PRE-VerificationBoundingBox@      * @arn:aws:lambda:ap-south-1:565803892007:function:PRE-VerificationSemanticSegmentation@      * @arn:aws:lambda:ap-south-1:565803892007:function:PRE-AdjustmentBoundingBox@      * @arn:aws:lambda:ap-south-1:565803892007:function:PRE-AdjustmentSemanticSegmentation@  __Asia Pacific (Singapore) (ap-southeast-1):__      * @arn:aws:lambda:ap-southeast-1:377565633583:function:PRE-BoundingBox@      * @arn:aws:lambda:ap-southeast-1:377565633583:function:PRE-ImageMultiClass@      * @arn:aws:lambda:ap-southeast-1:377565633583:function:PRE-ImageMultiClassMultiLabel@      * @arn:aws:lambda:ap-southeast-1:377565633583:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:ap-southeast-1:377565633583:function:PRE-TextMultiClass@      * @arn:aws:lambda:ap-southeast-1:377565633583:function:PRE-TextMultiClassMultiLabel@      * @arn:aws:lambda:ap-southeast-1:377565633583:function:PRE-NamedEntityRecognition@      * @arn:aws:lambda:ap-southeast-1:377565633583:function:PRE-VerificationBoundingBox@      * @arn:aws:lambda:ap-southeast-1:377565633583:function:PRE-VerificationSemanticSegmentation@      * @arn:aws:lambda:ap-southeast-1:377565633583:function:PRE-AdjustmentBoundingBox@      * @arn:aws:lambda:ap-southeast-1:377565633583:function:PRE-AdjustmentSemanticSegmentation@  __Asia Pacific (Sydney) (ap-southeast-2):__      * @arn:aws:lambda:ap-southeast-2:454466003867:function:PRE-BoundingBox@      * @arn:aws:lambda:ap-southeast-2:454466003867:function:PRE-ImageMultiClass@      * @arn:aws:lambda:ap-southeast-2:454466003867:function:PRE-ImageMultiClassMultiLabel@      * @arn:aws:lambda:ap-southeast-2:454466003867:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:ap-southeast-2:454466003867:function:PRE-TextMultiClass@      * @arn:aws:lambda:ap-southeast-2:454466003867:function:PRE-TextMultiClassMultiLabel@      * @arn:aws:lambda:ap-southeast-2:454466003867:function:PRE-NamedEntityRecognition@      * @arn:aws:lambda:ap-southeast-2:454466003867:function:PRE-VerificationBoundingBox@      * @arn:aws:lambda:ap-southeast-2:454466003867:function:PRE-VerificationSemanticSegmentation@      * @arn:aws:lambda:ap-southeast-2:454466003867:function:PRE-AdjustmentBoundingBox@      * @arn:aws:lambda:ap-southeast-2:454466003867:function:PRE-AdjustmentSemanticSegmentation@ 
htcPreHumanTaskLambdaARN :: Lens' HumanTaskConfig Text
htcPreHumanTaskLambdaARN = lens _htcPreHumanTaskLambdaARN (\ s a -> s{_htcPreHumanTaskLambdaARN = a})

-- | A title for the task for your human workers.
htcTaskTitle :: Lens' HumanTaskConfig Text
htcTaskTitle = lens _htcTaskTitle (\ s a -> s{_htcTaskTitle = a})

-- | A description of the task for your human workers.
htcTaskDescription :: Lens' HumanTaskConfig Text
htcTaskDescription = lens _htcTaskDescription (\ s a -> s{_htcTaskDescription = a})

-- | The number of human workers that will label an object. 
htcNumberOfHumanWorkersPerDataObject :: Lens' HumanTaskConfig Natural
htcNumberOfHumanWorkersPerDataObject = lens _htcNumberOfHumanWorkersPerDataObject (\ s a -> s{_htcNumberOfHumanWorkersPerDataObject = a}) . _Nat

-- | The amount of time that a worker has to complete a task.
htcTaskTimeLimitInSeconds :: Lens' HumanTaskConfig Natural
htcTaskTimeLimitInSeconds = lens _htcTaskTimeLimitInSeconds (\ s a -> s{_htcTaskTimeLimitInSeconds = a}) . _Nat

-- | Configures how labels are consolidated across human workers.
htcAnnotationConsolidationConfig :: Lens' HumanTaskConfig AnnotationConsolidationConfig
htcAnnotationConsolidationConfig = lens _htcAnnotationConsolidationConfig (\ s a -> s{_htcAnnotationConsolidationConfig = a})

instance FromJSON HumanTaskConfig where
        parseJSON
          = withObject "HumanTaskConfig"
              (\ x ->
                 HumanTaskConfig' <$>
                   (x .:? "TaskKeywords") <*>
                     (x .:? "PublicWorkforceTaskPrice")
                     <*> (x .:? "TaskAvailabilityLifetimeInSeconds")
                     <*> (x .:? "MaxConcurrentTaskCount")
                     <*> (x .: "WorkteamArn")
                     <*> (x .: "UiConfig")
                     <*> (x .: "PreHumanTaskLambdaArn")
                     <*> (x .: "TaskTitle")
                     <*> (x .: "TaskDescription")
                     <*> (x .: "NumberOfHumanWorkersPerDataObject")
                     <*> (x .: "TaskTimeLimitInSeconds")
                     <*> (x .: "AnnotationConsolidationConfig"))

instance Hashable HumanTaskConfig where

instance NFData HumanTaskConfig where

instance ToJSON HumanTaskConfig where
        toJSON HumanTaskConfig'{..}
          = object
              (catMaybes
                 [("TaskKeywords" .=) <$> _htcTaskKeywords,
                  ("PublicWorkforceTaskPrice" .=) <$>
                    _htcPublicWorkforceTaskPrice,
                  ("TaskAvailabilityLifetimeInSeconds" .=) <$>
                    _htcTaskAvailabilityLifetimeInSeconds,
                  ("MaxConcurrentTaskCount" .=) <$>
                    _htcMaxConcurrentTaskCount,
                  Just ("WorkteamArn" .= _htcWorkteamARN),
                  Just ("UiConfig" .= _htcUiConfig),
                  Just
                    ("PreHumanTaskLambdaArn" .=
                       _htcPreHumanTaskLambdaARN),
                  Just ("TaskTitle" .= _htcTaskTitle),
                  Just ("TaskDescription" .= _htcTaskDescription),
                  Just
                    ("NumberOfHumanWorkersPerDataObject" .=
                       _htcNumberOfHumanWorkersPerDataObject),
                  Just
                    ("TaskTimeLimitInSeconds" .=
                       _htcTaskTimeLimitInSeconds),
                  Just
                    ("AnnotationConsolidationConfig" .=
                       _htcAnnotationConsolidationConfig)])
