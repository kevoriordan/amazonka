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
-- Module      : Network.AWS.SWF.RespondActivityTaskCompleted
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used by workers to tell the service that the 'ActivityTask' identified by the @taskToken@ completed successfully with a @result@ (if provided). The @result@ appears in the @ActivityTaskCompleted@ event in the workflow history.
--
--
-- /Important:/ If the requested task doesn't complete successfully, use 'RespondActivityTaskFailed' instead. If the worker finds that the task is canceled through the @canceled@ flag returned by 'RecordActivityTaskHeartbeat' , it should cancel the task, clean up and then call 'RespondActivityTaskCanceled' .
--
-- A task is considered open from the time that it is scheduled until it is closed. Therefore a task is reported as open while a worker is processing it. A task is closed after it has been specified in a call to RespondActivityTaskCompleted, 'RespondActivityTaskCanceled' , 'RespondActivityTaskFailed' , or the task has <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dg-basic.html#swf-dev-timeout-types timed out> .
--
-- __Access Control__ 
--
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--     * You cannot use an IAM policy to constrain this action's parameters.
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
module Network.AWS.SWF.RespondActivityTaskCompleted
    (
    -- * Creating a Request
      respondActivityTaskCompleted
    , RespondActivityTaskCompleted
    -- * Request Lenses
    , ratcResult
    , ratcTaskToken

    -- * Destructuring the Response
    , respondActivityTaskCompletedResponse
    , RespondActivityTaskCompletedResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SWF.Types
import Network.AWS.SWF.Types.Product

-- | /See:/ 'respondActivityTaskCompleted' smart constructor.
data RespondActivityTaskCompleted = RespondActivityTaskCompleted'{_ratcResult
                                                                  ::
                                                                  !(Maybe Text),
                                                                  _ratcTaskToken
                                                                  :: !Text}
                                      deriving (Eq, Read, Show, Data, Typeable,
                                                Generic)

-- | Creates a value of 'RespondActivityTaskCompleted' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ratcResult' - The result of the activity task. It is a free form string that is implementation specific.
--
-- * 'ratcTaskToken' - The @taskToken@ of the 'ActivityTask' . /Important:/ @taskToken@ is generated by the service and should be treated as an opaque value. If the task is passed to another process, its @taskToken@ must also be passed. This enables it to provide its progress and respond with results.
respondActivityTaskCompleted
    :: Text -- ^ 'ratcTaskToken'
    -> RespondActivityTaskCompleted
respondActivityTaskCompleted pTaskToken_
  = RespondActivityTaskCompleted'{_ratcResult =
                                    Nothing,
                                  _ratcTaskToken = pTaskToken_}

-- | The result of the activity task. It is a free form string that is implementation specific.
ratcResult :: Lens' RespondActivityTaskCompleted (Maybe Text)
ratcResult = lens _ratcResult (\ s a -> s{_ratcResult = a})

-- | The @taskToken@ of the 'ActivityTask' . /Important:/ @taskToken@ is generated by the service and should be treated as an opaque value. If the task is passed to another process, its @taskToken@ must also be passed. This enables it to provide its progress and respond with results.
ratcTaskToken :: Lens' RespondActivityTaskCompleted Text
ratcTaskToken = lens _ratcTaskToken (\ s a -> s{_ratcTaskToken = a})

instance AWSRequest RespondActivityTaskCompleted
         where
        type Rs RespondActivityTaskCompleted =
             RespondActivityTaskCompletedResponse
        request = postJSON swf
        response
          = receiveNull RespondActivityTaskCompletedResponse'

instance Hashable RespondActivityTaskCompleted where

instance NFData RespondActivityTaskCompleted where

instance ToHeaders RespondActivityTaskCompleted where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SimpleWorkflowService.RespondActivityTaskCompleted"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON RespondActivityTaskCompleted where
        toJSON RespondActivityTaskCompleted'{..}
          = object
              (catMaybes
                 [("result" .=) <$> _ratcResult,
                  Just ("taskToken" .= _ratcTaskToken)])

instance ToPath RespondActivityTaskCompleted where
        toPath = const "/"

instance ToQuery RespondActivityTaskCompleted where
        toQuery = const mempty

-- | /See:/ 'respondActivityTaskCompletedResponse' smart constructor.
data RespondActivityTaskCompletedResponse = RespondActivityTaskCompletedResponse'
                                              deriving (Eq, Read, Show, Data,
                                                        Typeable, Generic)

-- | Creates a value of 'RespondActivityTaskCompletedResponse' with the minimum fields required to make a request.
--
respondActivityTaskCompletedResponse
    :: RespondActivityTaskCompletedResponse
respondActivityTaskCompletedResponse
  = RespondActivityTaskCompletedResponse'

instance NFData RespondActivityTaskCompletedResponse
         where
