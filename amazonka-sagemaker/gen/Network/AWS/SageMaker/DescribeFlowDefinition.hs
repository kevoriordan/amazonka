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
-- Module      : Network.AWS.SageMaker.DescribeFlowDefinition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified flow definition.
--
--
module Network.AWS.SageMaker.DescribeFlowDefinition
    (
    -- * Creating a Request
      describeFlowDefinition
    , DescribeFlowDefinition
    -- * Request Lenses
    , dFlowDefinitionName

    -- * Destructuring the Response
    , describeFlowDefinitionResponse
    , DescribeFlowDefinitionResponse
    -- * Response Lenses
    , dscrbflwdfntnrsFailureReason
    , dscrbflwdfntnrsHumanLoopRequestSource
    , dscrbflwdfntnrsHumanLoopActivationConfig
    , dscrbflwdfntnrsResponseStatus
    , dscrbflwdfntnrsFlowDefinitionARN
    , dscrbflwdfntnrsFlowDefinitionName
    , dscrbflwdfntnrsFlowDefinitionStatus
    , dscrbflwdfntnrsCreationTime
    , dscrbflwdfntnrsHumanLoopConfig
    , dscrbflwdfntnrsOutputConfig
    , dscrbflwdfntnrsRoleARN
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'describeFlowDefinition' smart constructor.
newtype DescribeFlowDefinition = DescribeFlowDefinition'{_dFlowDefinitionName
                                                         :: Text}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'DescribeFlowDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dFlowDefinitionName' - The name of the flow definition.
describeFlowDefinition
    :: Text -- ^ 'dFlowDefinitionName'
    -> DescribeFlowDefinition
describeFlowDefinition pFlowDefinitionName_
  = DescribeFlowDefinition'{_dFlowDefinitionName =
                              pFlowDefinitionName_}

-- | The name of the flow definition.
dFlowDefinitionName :: Lens' DescribeFlowDefinition Text
dFlowDefinitionName = lens _dFlowDefinitionName (\ s a -> s{_dFlowDefinitionName = a})

instance AWSRequest DescribeFlowDefinition where
        type Rs DescribeFlowDefinition =
             DescribeFlowDefinitionResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 DescribeFlowDefinitionResponse' <$>
                   (x .?> "FailureReason") <*>
                     (x .?> "HumanLoopRequestSource")
                     <*> (x .?> "HumanLoopActivationConfig")
                     <*> (pure (fromEnum s))
                     <*> (x .:> "FlowDefinitionArn")
                     <*> (x .:> "FlowDefinitionName")
                     <*> (x .:> "FlowDefinitionStatus")
                     <*> (x .:> "CreationTime")
                     <*> (x .:> "HumanLoopConfig")
                     <*> (x .:> "OutputConfig")
                     <*> (x .:> "RoleArn"))

instance Hashable DescribeFlowDefinition where

instance NFData DescribeFlowDefinition where

instance ToHeaders DescribeFlowDefinition where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.DescribeFlowDefinition" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeFlowDefinition where
        toJSON DescribeFlowDefinition'{..}
          = object
              (catMaybes
                 [Just
                    ("FlowDefinitionName" .= _dFlowDefinitionName)])

instance ToPath DescribeFlowDefinition where
        toPath = const "/"

instance ToQuery DescribeFlowDefinition where
        toQuery = const mempty

-- | /See:/ 'describeFlowDefinitionResponse' smart constructor.
data DescribeFlowDefinitionResponse = DescribeFlowDefinitionResponse'{_dscrbflwdfntnrsFailureReason
                                                                      ::
                                                                      !(Maybe
                                                                          Text),
                                                                      _dscrbflwdfntnrsHumanLoopRequestSource
                                                                      ::
                                                                      !(Maybe
                                                                          HumanLoopRequestSource),
                                                                      _dscrbflwdfntnrsHumanLoopActivationConfig
                                                                      ::
                                                                      !(Maybe
                                                                          HumanLoopActivationConfig),
                                                                      _dscrbflwdfntnrsResponseStatus
                                                                      :: !Int,
                                                                      _dscrbflwdfntnrsFlowDefinitionARN
                                                                      :: !Text,
                                                                      _dscrbflwdfntnrsFlowDefinitionName
                                                                      :: !Text,
                                                                      _dscrbflwdfntnrsFlowDefinitionStatus
                                                                      ::
                                                                      !FlowDefinitionStatus,
                                                                      _dscrbflwdfntnrsCreationTime
                                                                      :: !POSIX,
                                                                      _dscrbflwdfntnrsHumanLoopConfig
                                                                      ::
                                                                      !HumanLoopConfig,
                                                                      _dscrbflwdfntnrsOutputConfig
                                                                      ::
                                                                      !FlowDefinitionOutputConfig,
                                                                      _dscrbflwdfntnrsRoleARN
                                                                      :: !Text}
                                        deriving (Eq, Read, Show, Data,
                                                  Typeable, Generic)

-- | Creates a value of 'DescribeFlowDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscrbflwdfntnrsFailureReason' - 
--
-- * 'dscrbflwdfntnrsHumanLoopRequestSource' - Container for configuring the source of human task requests. Used to specify if Amazon Rekognition or Amazon Textract is used as an integration source.
--
-- * 'dscrbflwdfntnrsHumanLoopActivationConfig' - An object containing information about what triggers a human review workflow.
--
-- * 'dscrbflwdfntnrsResponseStatus' - -- | The response status code.
--
-- * 'dscrbflwdfntnrsFlowDefinitionARN' - The Amazon Resource Name (ARN) of the flow defintion.
--
-- * 'dscrbflwdfntnrsFlowDefinitionName' - The Amazon Resource Name (ARN) of the flow definition.
--
-- * 'dscrbflwdfntnrsFlowDefinitionStatus' - The status of the flow definition. Valid values are listed below.
--
-- * 'dscrbflwdfntnrsCreationTime' - The timestamp when the flow definition was created.
--
-- * 'dscrbflwdfntnrsHumanLoopConfig' - An object containing information about who works on the task, the workforce task price, and other task details.
--
-- * 'dscrbflwdfntnrsOutputConfig' - An object containing information about the output file.
--
-- * 'dscrbflwdfntnrsRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) execution role for the flow definition.
describeFlowDefinitionResponse
    :: Int -- ^ 'dscrbflwdfntnrsResponseStatus'
    -> Text -- ^ 'dscrbflwdfntnrsFlowDefinitionARN'
    -> Text -- ^ 'dscrbflwdfntnrsFlowDefinitionName'
    -> FlowDefinitionStatus -- ^ 'dscrbflwdfntnrsFlowDefinitionStatus'
    -> UTCTime -- ^ 'dscrbflwdfntnrsCreationTime'
    -> HumanLoopConfig -- ^ 'dscrbflwdfntnrsHumanLoopConfig'
    -> FlowDefinitionOutputConfig -- ^ 'dscrbflwdfntnrsOutputConfig'
    -> Text -- ^ 'dscrbflwdfntnrsRoleARN'
    -> DescribeFlowDefinitionResponse
describeFlowDefinitionResponse pResponseStatus_
  pFlowDefinitionARN_ pFlowDefinitionName_
  pFlowDefinitionStatus_ pCreationTime_
  pHumanLoopConfig_ pOutputConfig_ pRoleARN_
  = DescribeFlowDefinitionResponse'{_dscrbflwdfntnrsFailureReason
                                      = Nothing,
                                    _dscrbflwdfntnrsHumanLoopRequestSource =
                                      Nothing,
                                    _dscrbflwdfntnrsHumanLoopActivationConfig =
                                      Nothing,
                                    _dscrbflwdfntnrsResponseStatus =
                                      pResponseStatus_,
                                    _dscrbflwdfntnrsFlowDefinitionARN =
                                      pFlowDefinitionARN_,
                                    _dscrbflwdfntnrsFlowDefinitionName =
                                      pFlowDefinitionName_,
                                    _dscrbflwdfntnrsFlowDefinitionStatus =
                                      pFlowDefinitionStatus_,
                                    _dscrbflwdfntnrsCreationTime =
                                      _Time # pCreationTime_,
                                    _dscrbflwdfntnrsHumanLoopConfig =
                                      pHumanLoopConfig_,
                                    _dscrbflwdfntnrsOutputConfig =
                                      pOutputConfig_,
                                    _dscrbflwdfntnrsRoleARN = pRoleARN_}

-- | 
dscrbflwdfntnrsFailureReason :: Lens' DescribeFlowDefinitionResponse (Maybe Text)
dscrbflwdfntnrsFailureReason = lens _dscrbflwdfntnrsFailureReason (\ s a -> s{_dscrbflwdfntnrsFailureReason = a})

-- | Container for configuring the source of human task requests. Used to specify if Amazon Rekognition or Amazon Textract is used as an integration source.
dscrbflwdfntnrsHumanLoopRequestSource :: Lens' DescribeFlowDefinitionResponse (Maybe HumanLoopRequestSource)
dscrbflwdfntnrsHumanLoopRequestSource = lens _dscrbflwdfntnrsHumanLoopRequestSource (\ s a -> s{_dscrbflwdfntnrsHumanLoopRequestSource = a})

-- | An object containing information about what triggers a human review workflow.
dscrbflwdfntnrsHumanLoopActivationConfig :: Lens' DescribeFlowDefinitionResponse (Maybe HumanLoopActivationConfig)
dscrbflwdfntnrsHumanLoopActivationConfig = lens _dscrbflwdfntnrsHumanLoopActivationConfig (\ s a -> s{_dscrbflwdfntnrsHumanLoopActivationConfig = a})

-- | -- | The response status code.
dscrbflwdfntnrsResponseStatus :: Lens' DescribeFlowDefinitionResponse Int
dscrbflwdfntnrsResponseStatus = lens _dscrbflwdfntnrsResponseStatus (\ s a -> s{_dscrbflwdfntnrsResponseStatus = a})

-- | The Amazon Resource Name (ARN) of the flow defintion.
dscrbflwdfntnrsFlowDefinitionARN :: Lens' DescribeFlowDefinitionResponse Text
dscrbflwdfntnrsFlowDefinitionARN = lens _dscrbflwdfntnrsFlowDefinitionARN (\ s a -> s{_dscrbflwdfntnrsFlowDefinitionARN = a})

-- | The Amazon Resource Name (ARN) of the flow definition.
dscrbflwdfntnrsFlowDefinitionName :: Lens' DescribeFlowDefinitionResponse Text
dscrbflwdfntnrsFlowDefinitionName = lens _dscrbflwdfntnrsFlowDefinitionName (\ s a -> s{_dscrbflwdfntnrsFlowDefinitionName = a})

-- | The status of the flow definition. Valid values are listed below.
dscrbflwdfntnrsFlowDefinitionStatus :: Lens' DescribeFlowDefinitionResponse FlowDefinitionStatus
dscrbflwdfntnrsFlowDefinitionStatus = lens _dscrbflwdfntnrsFlowDefinitionStatus (\ s a -> s{_dscrbflwdfntnrsFlowDefinitionStatus = a})

-- | The timestamp when the flow definition was created.
dscrbflwdfntnrsCreationTime :: Lens' DescribeFlowDefinitionResponse UTCTime
dscrbflwdfntnrsCreationTime = lens _dscrbflwdfntnrsCreationTime (\ s a -> s{_dscrbflwdfntnrsCreationTime = a}) . _Time

-- | An object containing information about who works on the task, the workforce task price, and other task details.
dscrbflwdfntnrsHumanLoopConfig :: Lens' DescribeFlowDefinitionResponse HumanLoopConfig
dscrbflwdfntnrsHumanLoopConfig = lens _dscrbflwdfntnrsHumanLoopConfig (\ s a -> s{_dscrbflwdfntnrsHumanLoopConfig = a})

-- | An object containing information about the output file.
dscrbflwdfntnrsOutputConfig :: Lens' DescribeFlowDefinitionResponse FlowDefinitionOutputConfig
dscrbflwdfntnrsOutputConfig = lens _dscrbflwdfntnrsOutputConfig (\ s a -> s{_dscrbflwdfntnrsOutputConfig = a})

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) execution role for the flow definition.
dscrbflwdfntnrsRoleARN :: Lens' DescribeFlowDefinitionResponse Text
dscrbflwdfntnrsRoleARN = lens _dscrbflwdfntnrsRoleARN (\ s a -> s{_dscrbflwdfntnrsRoleARN = a})

instance NFData DescribeFlowDefinitionResponse where
