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
-- Module      : Network.AWS.Glue.CreateJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new job definition.
--
--
module Network.AWS.Glue.CreateJob
    (
    -- * Creating a Request
      createJob
    , CreateJob
    -- * Request Lenses
    , cjNotificationProperty
    , cjConnections
    , cjSecurityConfiguration
    , cjLogURI
    , cjMaxRetries
    , cjExecutionProperty
    , cjAllocatedCapacity
    , cjMaxCapacity
    , cjTimeout
    , cjDefaultArguments
    , cjDescription
    , cjName
    , cjRole
    , cjCommand

    -- * Destructuring the Response
    , createJobResponse
    , CreateJobResponse
    -- * Response Lenses
    , cjrsName
    , cjrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createJob' smart constructor.
data CreateJob = CreateJob'
  { _cjNotificationProperty  :: !(Maybe NotificationProperty)
  , _cjConnections           :: !(Maybe ConnectionsList)
  , _cjSecurityConfiguration :: !(Maybe Text)
  , _cjLogURI                :: !(Maybe Text)
  , _cjMaxRetries            :: !(Maybe Int)
  , _cjExecutionProperty     :: !(Maybe ExecutionProperty)
  , _cjAllocatedCapacity     :: !(Maybe Int)
  , _cjMaxCapacity           :: !(Maybe Double)
  , _cjTimeout               :: !(Maybe Nat)
  , _cjDefaultArguments      :: !(Maybe (Map Text Text))
  , _cjDescription           :: !(Maybe Text)
  , _cjName                  :: !Text
  , _cjRole                  :: !Text
  , _cjCommand               :: !JobCommand
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjNotificationProperty' - Specifies configuration properties of a job notification.
--
-- * 'cjConnections' - The connections used for this job.
--
-- * 'cjSecurityConfiguration' - The name of the SecurityConfiguration structure to be used with this job.
--
-- * 'cjLogURI' - This field is reserved for future use.
--
-- * 'cjMaxRetries' - The maximum number of times to retry this job if it fails.
--
-- * 'cjExecutionProperty' - An ExecutionProperty specifying the maximum number of concurrent runs allowed for this job.
--
-- * 'cjAllocatedCapacity' - This parameter is deprecated. Use @MaxCapacity@ instead. The number of AWS Glue data processing units (DPUs) to allocate to this Job. From 2 to 100 DPUs can be allocated; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
--
-- * 'cjMaxCapacity' - AWS Glue supports running jobs on a @JobCommand.Name@ ="pythonshell" with allocated processing as low as 0.0625 DPU, which can be specified using @MaxCapacity@ . Glue ETL jobs running in any other way cannot have fractional DPU allocations.
--
-- * 'cjTimeout' - The job timeout in minutes. This is the maximum time that a job run can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours).
--
-- * 'cjDefaultArguments' - The default arguments for this job. You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes. For information about how to specify and consume your own Job arguments, see the <http://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide. For information about the key-value pairs that AWS Glue consumes to set up your job, see the <http://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
--
-- * 'cjDescription' - Description of the job being defined.
--
-- * 'cjName' - The name you assign to this job definition. It must be unique in your account.
--
-- * 'cjRole' - The name or ARN of the IAM role associated with this job.
--
-- * 'cjCommand' - The JobCommand that executes this job.
createJob
    :: Text -- ^ 'cjName'
    -> Text -- ^ 'cjRole'
    -> JobCommand -- ^ 'cjCommand'
    -> CreateJob
createJob pName_ pRole_ pCommand_ =
  CreateJob'
    { _cjNotificationProperty = Nothing
    , _cjConnections = Nothing
    , _cjSecurityConfiguration = Nothing
    , _cjLogURI = Nothing
    , _cjMaxRetries = Nothing
    , _cjExecutionProperty = Nothing
    , _cjAllocatedCapacity = Nothing
    , _cjMaxCapacity = Nothing
    , _cjTimeout = Nothing
    , _cjDefaultArguments = Nothing
    , _cjDescription = Nothing
    , _cjName = pName_
    , _cjRole = pRole_
    , _cjCommand = pCommand_
    }


-- | Specifies configuration properties of a job notification.
cjNotificationProperty :: Lens' CreateJob (Maybe NotificationProperty)
cjNotificationProperty = lens _cjNotificationProperty (\ s a -> s{_cjNotificationProperty = a})

-- | The connections used for this job.
cjConnections :: Lens' CreateJob (Maybe ConnectionsList)
cjConnections = lens _cjConnections (\ s a -> s{_cjConnections = a})

-- | The name of the SecurityConfiguration structure to be used with this job.
cjSecurityConfiguration :: Lens' CreateJob (Maybe Text)
cjSecurityConfiguration = lens _cjSecurityConfiguration (\ s a -> s{_cjSecurityConfiguration = a})

-- | This field is reserved for future use.
cjLogURI :: Lens' CreateJob (Maybe Text)
cjLogURI = lens _cjLogURI (\ s a -> s{_cjLogURI = a})

-- | The maximum number of times to retry this job if it fails.
cjMaxRetries :: Lens' CreateJob (Maybe Int)
cjMaxRetries = lens _cjMaxRetries (\ s a -> s{_cjMaxRetries = a})

-- | An ExecutionProperty specifying the maximum number of concurrent runs allowed for this job.
cjExecutionProperty :: Lens' CreateJob (Maybe ExecutionProperty)
cjExecutionProperty = lens _cjExecutionProperty (\ s a -> s{_cjExecutionProperty = a})

-- | This parameter is deprecated. Use @MaxCapacity@ instead. The number of AWS Glue data processing units (DPUs) to allocate to this Job. From 2 to 100 DPUs can be allocated; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
cjAllocatedCapacity :: Lens' CreateJob (Maybe Int)
cjAllocatedCapacity = lens _cjAllocatedCapacity (\ s a -> s{_cjAllocatedCapacity = a})

-- | AWS Glue supports running jobs on a @JobCommand.Name@ ="pythonshell" with allocated processing as low as 0.0625 DPU, which can be specified using @MaxCapacity@ . Glue ETL jobs running in any other way cannot have fractional DPU allocations.
cjMaxCapacity :: Lens' CreateJob (Maybe Double)
cjMaxCapacity = lens _cjMaxCapacity (\ s a -> s{_cjMaxCapacity = a})

-- | The job timeout in minutes. This is the maximum time that a job run can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours).
cjTimeout :: Lens' CreateJob (Maybe Natural)
cjTimeout = lens _cjTimeout (\ s a -> s{_cjTimeout = a}) . mapping _Nat

-- | The default arguments for this job. You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes. For information about how to specify and consume your own Job arguments, see the <http://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide. For information about the key-value pairs that AWS Glue consumes to set up your job, see the <http://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
cjDefaultArguments :: Lens' CreateJob (HashMap Text Text)
cjDefaultArguments = lens _cjDefaultArguments (\ s a -> s{_cjDefaultArguments = a}) . _Default . _Map

-- | Description of the job being defined.
cjDescription :: Lens' CreateJob (Maybe Text)
cjDescription = lens _cjDescription (\ s a -> s{_cjDescription = a})

-- | The name you assign to this job definition. It must be unique in your account.
cjName :: Lens' CreateJob Text
cjName = lens _cjName (\ s a -> s{_cjName = a})

-- | The name or ARN of the IAM role associated with this job.
cjRole :: Lens' CreateJob Text
cjRole = lens _cjRole (\ s a -> s{_cjRole = a})

-- | The JobCommand that executes this job.
cjCommand :: Lens' CreateJob JobCommand
cjCommand = lens _cjCommand (\ s a -> s{_cjCommand = a})

instance AWSRequest CreateJob where
        type Rs CreateJob = CreateJobResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 CreateJobResponse' <$>
                   (x .?> "Name") <*> (pure (fromEnum s)))

instance Hashable CreateJob where

instance NFData CreateJob where

instance ToHeaders CreateJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.CreateJob" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateJob where
        toJSON CreateJob'{..}
          = object
              (catMaybes
                 [("NotificationProperty" .=) <$>
                    _cjNotificationProperty,
                  ("Connections" .=) <$> _cjConnections,
                  ("SecurityConfiguration" .=) <$>
                    _cjSecurityConfiguration,
                  ("LogUri" .=) <$> _cjLogURI,
                  ("MaxRetries" .=) <$> _cjMaxRetries,
                  ("ExecutionProperty" .=) <$> _cjExecutionProperty,
                  ("AllocatedCapacity" .=) <$> _cjAllocatedCapacity,
                  ("MaxCapacity" .=) <$> _cjMaxCapacity,
                  ("Timeout" .=) <$> _cjTimeout,
                  ("DefaultArguments" .=) <$> _cjDefaultArguments,
                  ("Description" .=) <$> _cjDescription,
                  Just ("Name" .= _cjName), Just ("Role" .= _cjRole),
                  Just ("Command" .= _cjCommand)])

instance ToPath CreateJob where
        toPath = const "/"

instance ToQuery CreateJob where
        toQuery = const mempty

-- | /See:/ 'createJobResponse' smart constructor.
data CreateJobResponse = CreateJobResponse'
  { _cjrsName           :: !(Maybe Text)
  , _cjrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjrsName' - The unique name that was provided for this job definition.
--
-- * 'cjrsResponseStatus' - -- | The response status code.
createJobResponse
    :: Int -- ^ 'cjrsResponseStatus'
    -> CreateJobResponse
createJobResponse pResponseStatus_ =
  CreateJobResponse'
    {_cjrsName = Nothing, _cjrsResponseStatus = pResponseStatus_}


-- | The unique name that was provided for this job definition.
cjrsName :: Lens' CreateJobResponse (Maybe Text)
cjrsName = lens _cjrsName (\ s a -> s{_cjrsName = a})

-- | -- | The response status code.
cjrsResponseStatus :: Lens' CreateJobResponse Int
cjrsResponseStatus = lens _cjrsResponseStatus (\ s a -> s{_cjrsResponseStatus = a})

instance NFData CreateJobResponse where
