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
-- Module      : Network.AWS.CloudFormation.DescribeChangeSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the inputs for the change set and a list of changes that AWS CloudFormation will make if you execute the change set. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-updating-stacks-changesets.html Updating Stacks Using Change Sets> in the AWS CloudFormation User Guide.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.DescribeChangeSet
    (
    -- * Creating a Request
      describeChangeSet
    , DescribeChangeSet
    -- * Request Lenses
    , desNextToken
    , desStackName
    , desChangeSetName

    -- * Destructuring the Response
    , describeChangeSetResponse
    , DescribeChangeSetResponse
    -- * Response Lenses
    , dscrbchngstrsCreationTime
    , dscrbchngstrsChanges
    , dscrbchngstrsNotificationARNs
    , dscrbchngstrsChangeSetName
    , dscrbchngstrsExecutionStatus
    , dscrbchngstrsChangeSetId
    , dscrbchngstrsNextToken
    , dscrbchngstrsParameters
    , dscrbchngstrsStatusReason
    , dscrbchngstrsStackId
    , dscrbchngstrsDescription
    , dscrbchngstrsCapabilities
    , dscrbchngstrsRollbackConfiguration
    , dscrbchngstrsTags
    , dscrbchngstrsStackName
    , dscrbchngstrsResponseStatus
    , dscrbchngstrsStatus
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the 'DescribeChangeSet' action.
--
--
--
-- /See:/ 'describeChangeSet' smart constructor.
data DescribeChangeSet = DescribeChangeSet'{_desNextToken
                                            :: !(Maybe Text),
                                            _desStackName :: !(Maybe Text),
                                            _desChangeSetName :: !Text}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeChangeSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desNextToken' - A string (provided by the 'DescribeChangeSet' response output) that identifies the next page of information that you want to retrieve.
--
-- * 'desStackName' - If you specified the name of a change set, specify the stack name or ID (ARN) of the change set you want to describe.
--
-- * 'desChangeSetName' - The name or Amazon Resource Name (ARN) of the change set that you want to describe.
describeChangeSet
    :: Text -- ^ 'desChangeSetName'
    -> DescribeChangeSet
describeChangeSet pChangeSetName_
  = DescribeChangeSet'{_desNextToken = Nothing,
                       _desStackName = Nothing,
                       _desChangeSetName = pChangeSetName_}

-- | A string (provided by the 'DescribeChangeSet' response output) that identifies the next page of information that you want to retrieve.
desNextToken :: Lens' DescribeChangeSet (Maybe Text)
desNextToken = lens _desNextToken (\ s a -> s{_desNextToken = a})

-- | If you specified the name of a change set, specify the stack name or ID (ARN) of the change set you want to describe.
desStackName :: Lens' DescribeChangeSet (Maybe Text)
desStackName = lens _desStackName (\ s a -> s{_desStackName = a})

-- | The name or Amazon Resource Name (ARN) of the change set that you want to describe.
desChangeSetName :: Lens' DescribeChangeSet Text
desChangeSetName = lens _desChangeSetName (\ s a -> s{_desChangeSetName = a})

instance AWSPager DescribeChangeSet where
        page rq rs
          | stop (rs ^. dscrbchngstrsNextToken) = Nothing
          | stop (rs ^. dscrbchngstrsChanges) = Nothing
          | otherwise =
            Just $ rq &
              desNextToken .~ rs ^. dscrbchngstrsNextToken

instance AWSRequest DescribeChangeSet where
        type Rs DescribeChangeSet = DescribeChangeSetResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "DescribeChangeSetResult"
              (\ s h x ->
                 DescribeChangeSetResponse' <$>
                   (x .@? "CreationTime") <*>
                     (x .@? "Changes" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*>
                     (x .@? "NotificationARNs" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (x .@? "ChangeSetName")
                     <*> (x .@? "ExecutionStatus")
                     <*> (x .@? "ChangeSetId")
                     <*> (x .@? "NextToken")
                     <*>
                     (x .@? "Parameters" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (x .@? "StatusReason")
                     <*> (x .@? "StackId")
                     <*> (x .@? "Description")
                     <*>
                     (x .@? "Capabilities" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (x .@? "RollbackConfiguration")
                     <*>
                     (x .@? "Tags" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (x .@? "StackName")
                     <*> (pure (fromEnum s))
                     <*> (x .@ "Status"))

instance Hashable DescribeChangeSet where

instance NFData DescribeChangeSet where

instance ToHeaders DescribeChangeSet where
        toHeaders = const mempty

instance ToPath DescribeChangeSet where
        toPath = const "/"

instance ToQuery DescribeChangeSet where
        toQuery DescribeChangeSet'{..}
          = mconcat
              ["Action" =: ("DescribeChangeSet" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "NextToken" =: _desNextToken,
               "StackName" =: _desStackName,
               "ChangeSetName" =: _desChangeSetName]

-- | The output for the 'DescribeChangeSet' action.
--
--
--
-- /See:/ 'describeChangeSetResponse' smart constructor.
data DescribeChangeSetResponse = DescribeChangeSetResponse'{_dscrbchngstrsCreationTime
                                                            :: !(Maybe ISO8601),
                                                            _dscrbchngstrsChanges
                                                            ::
                                                            !(Maybe [Change]),
                                                            _dscrbchngstrsNotificationARNs
                                                            :: !(Maybe [Text]),
                                                            _dscrbchngstrsChangeSetName
                                                            :: !(Maybe Text),
                                                            _dscrbchngstrsExecutionStatus
                                                            ::
                                                            !(Maybe
                                                                ExecutionStatus),
                                                            _dscrbchngstrsChangeSetId
                                                            :: !(Maybe Text),
                                                            _dscrbchngstrsNextToken
                                                            :: !(Maybe Text),
                                                            _dscrbchngstrsParameters
                                                            ::
                                                            !(Maybe
                                                                [Parameter]),
                                                            _dscrbchngstrsStatusReason
                                                            :: !(Maybe Text),
                                                            _dscrbchngstrsStackId
                                                            :: !(Maybe Text),
                                                            _dscrbchngstrsDescription
                                                            :: !(Maybe Text),
                                                            _dscrbchngstrsCapabilities
                                                            ::
                                                            !(Maybe
                                                                [Capability]),
                                                            _dscrbchngstrsRollbackConfiguration
                                                            ::
                                                            !(Maybe
                                                                RollbackConfiguration),
                                                            _dscrbchngstrsTags
                                                            :: !(Maybe [Tag]),
                                                            _dscrbchngstrsStackName
                                                            :: !(Maybe Text),
                                                            _dscrbchngstrsResponseStatus
                                                            :: !Int,
                                                            _dscrbchngstrsStatus
                                                            :: !ChangeSetStatus}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'DescribeChangeSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscrbchngstrsCreationTime' - The start time when the change set was created, in UTC.
--
-- * 'dscrbchngstrsChanges' - A list of @Change@ structures that describes the resources AWS CloudFormation changes if you execute the change set.
--
-- * 'dscrbchngstrsNotificationARNs' - The ARNs of the Amazon Simple Notification Service (Amazon SNS) topics that will be associated with the stack if you execute the change set.
--
-- * 'dscrbchngstrsChangeSetName' - The name of the change set.
--
-- * 'dscrbchngstrsExecutionStatus' - If the change set execution status is @AVAILABLE@ , you can execute the change set. If you can’t execute the change set, the status indicates why. For example, a change set might be in an @UNAVAILABLE@ state because AWS CloudFormation is still creating it or in an @OBSOLETE@ state because the stack was already updated.
--
-- * 'dscrbchngstrsChangeSetId' - The ARN of the change set.
--
-- * 'dscrbchngstrsNextToken' - If the output exceeds 1 MB, a string that identifies the next page of changes. If there is no additional page, this value is null.
--
-- * 'dscrbchngstrsParameters' - A list of @Parameter@ structures that describes the input parameters and their values used to create the change set. For more information, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter> data type.
--
-- * 'dscrbchngstrsStatusReason' - A description of the change set's status. For example, if your attempt to create a change set failed, AWS CloudFormation shows the error message.
--
-- * 'dscrbchngstrsStackId' - The ARN of the stack that is associated with the change set.
--
-- * 'dscrbchngstrsDescription' - Information about the change set.
--
-- * 'dscrbchngstrsCapabilities' - If you execute the change set, the list of capabilities that were explicitly acknowledged when the change set was created.
--
-- * 'dscrbchngstrsRollbackConfiguration' - The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
--
-- * 'dscrbchngstrsTags' - If you execute the change set, the tags that will be associated with the stack.
--
-- * 'dscrbchngstrsStackName' - The name of the stack that is associated with the change set.
--
-- * 'dscrbchngstrsResponseStatus' - -- | The response status code.
--
-- * 'dscrbchngstrsStatus' - The current status of the change set, such as @CREATE_IN_PROGRESS@ , @CREATE_COMPLETE@ , or @FAILED@ .
describeChangeSetResponse
    :: Int -- ^ 'dscrbchngstrsResponseStatus'
    -> ChangeSetStatus -- ^ 'dscrbchngstrsStatus'
    -> DescribeChangeSetResponse
describeChangeSetResponse pResponseStatus_ pStatus_
  = DescribeChangeSetResponse'{_dscrbchngstrsCreationTime
                                 = Nothing,
                               _dscrbchngstrsChanges = Nothing,
                               _dscrbchngstrsNotificationARNs = Nothing,
                               _dscrbchngstrsChangeSetName = Nothing,
                               _dscrbchngstrsExecutionStatus = Nothing,
                               _dscrbchngstrsChangeSetId = Nothing,
                               _dscrbchngstrsNextToken = Nothing,
                               _dscrbchngstrsParameters = Nothing,
                               _dscrbchngstrsStatusReason = Nothing,
                               _dscrbchngstrsStackId = Nothing,
                               _dscrbchngstrsDescription = Nothing,
                               _dscrbchngstrsCapabilities = Nothing,
                               _dscrbchngstrsRollbackConfiguration = Nothing,
                               _dscrbchngstrsTags = Nothing,
                               _dscrbchngstrsStackName = Nothing,
                               _dscrbchngstrsResponseStatus = pResponseStatus_,
                               _dscrbchngstrsStatus = pStatus_}

-- | The start time when the change set was created, in UTC.
dscrbchngstrsCreationTime :: Lens' DescribeChangeSetResponse (Maybe UTCTime)
dscrbchngstrsCreationTime = lens _dscrbchngstrsCreationTime (\ s a -> s{_dscrbchngstrsCreationTime = a}) . mapping _Time

-- | A list of @Change@ structures that describes the resources AWS CloudFormation changes if you execute the change set.
dscrbchngstrsChanges :: Lens' DescribeChangeSetResponse [Change]
dscrbchngstrsChanges = lens _dscrbchngstrsChanges (\ s a -> s{_dscrbchngstrsChanges = a}) . _Default . _Coerce

-- | The ARNs of the Amazon Simple Notification Service (Amazon SNS) topics that will be associated with the stack if you execute the change set.
dscrbchngstrsNotificationARNs :: Lens' DescribeChangeSetResponse [Text]
dscrbchngstrsNotificationARNs = lens _dscrbchngstrsNotificationARNs (\ s a -> s{_dscrbchngstrsNotificationARNs = a}) . _Default . _Coerce

-- | The name of the change set.
dscrbchngstrsChangeSetName :: Lens' DescribeChangeSetResponse (Maybe Text)
dscrbchngstrsChangeSetName = lens _dscrbchngstrsChangeSetName (\ s a -> s{_dscrbchngstrsChangeSetName = a})

-- | If the change set execution status is @AVAILABLE@ , you can execute the change set. If you can’t execute the change set, the status indicates why. For example, a change set might be in an @UNAVAILABLE@ state because AWS CloudFormation is still creating it or in an @OBSOLETE@ state because the stack was already updated.
dscrbchngstrsExecutionStatus :: Lens' DescribeChangeSetResponse (Maybe ExecutionStatus)
dscrbchngstrsExecutionStatus = lens _dscrbchngstrsExecutionStatus (\ s a -> s{_dscrbchngstrsExecutionStatus = a})

-- | The ARN of the change set.
dscrbchngstrsChangeSetId :: Lens' DescribeChangeSetResponse (Maybe Text)
dscrbchngstrsChangeSetId = lens _dscrbchngstrsChangeSetId (\ s a -> s{_dscrbchngstrsChangeSetId = a})

-- | If the output exceeds 1 MB, a string that identifies the next page of changes. If there is no additional page, this value is null.
dscrbchngstrsNextToken :: Lens' DescribeChangeSetResponse (Maybe Text)
dscrbchngstrsNextToken = lens _dscrbchngstrsNextToken (\ s a -> s{_dscrbchngstrsNextToken = a})

-- | A list of @Parameter@ structures that describes the input parameters and their values used to create the change set. For more information, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter> data type.
dscrbchngstrsParameters :: Lens' DescribeChangeSetResponse [Parameter]
dscrbchngstrsParameters = lens _dscrbchngstrsParameters (\ s a -> s{_dscrbchngstrsParameters = a}) . _Default . _Coerce

-- | A description of the change set's status. For example, if your attempt to create a change set failed, AWS CloudFormation shows the error message.
dscrbchngstrsStatusReason :: Lens' DescribeChangeSetResponse (Maybe Text)
dscrbchngstrsStatusReason = lens _dscrbchngstrsStatusReason (\ s a -> s{_dscrbchngstrsStatusReason = a})

-- | The ARN of the stack that is associated with the change set.
dscrbchngstrsStackId :: Lens' DescribeChangeSetResponse (Maybe Text)
dscrbchngstrsStackId = lens _dscrbchngstrsStackId (\ s a -> s{_dscrbchngstrsStackId = a})

-- | Information about the change set.
dscrbchngstrsDescription :: Lens' DescribeChangeSetResponse (Maybe Text)
dscrbchngstrsDescription = lens _dscrbchngstrsDescription (\ s a -> s{_dscrbchngstrsDescription = a})

-- | If you execute the change set, the list of capabilities that were explicitly acknowledged when the change set was created.
dscrbchngstrsCapabilities :: Lens' DescribeChangeSetResponse [Capability]
dscrbchngstrsCapabilities = lens _dscrbchngstrsCapabilities (\ s a -> s{_dscrbchngstrsCapabilities = a}) . _Default . _Coerce

-- | The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
dscrbchngstrsRollbackConfiguration :: Lens' DescribeChangeSetResponse (Maybe RollbackConfiguration)
dscrbchngstrsRollbackConfiguration = lens _dscrbchngstrsRollbackConfiguration (\ s a -> s{_dscrbchngstrsRollbackConfiguration = a})

-- | If you execute the change set, the tags that will be associated with the stack.
dscrbchngstrsTags :: Lens' DescribeChangeSetResponse [Tag]
dscrbchngstrsTags = lens _dscrbchngstrsTags (\ s a -> s{_dscrbchngstrsTags = a}) . _Default . _Coerce

-- | The name of the stack that is associated with the change set.
dscrbchngstrsStackName :: Lens' DescribeChangeSetResponse (Maybe Text)
dscrbchngstrsStackName = lens _dscrbchngstrsStackName (\ s a -> s{_dscrbchngstrsStackName = a})

-- | -- | The response status code.
dscrbchngstrsResponseStatus :: Lens' DescribeChangeSetResponse Int
dscrbchngstrsResponseStatus = lens _dscrbchngstrsResponseStatus (\ s a -> s{_dscrbchngstrsResponseStatus = a})

-- | The current status of the change set, such as @CREATE_IN_PROGRESS@ , @CREATE_COMPLETE@ , or @FAILED@ .
dscrbchngstrsStatus :: Lens' DescribeChangeSetResponse ChangeSetStatus
dscrbchngstrsStatus = lens _dscrbchngstrsStatus (\ s a -> s{_dscrbchngstrsStatus = a})

instance NFData DescribeChangeSetResponse where
