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
-- Module      : Network.AWS.Glue.GetWorkflowRun
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the metadata for a given workflow run. 
--
--
module Network.AWS.Glue.GetWorkflowRun
    (
    -- * Creating a Request
      getWorkflowRun
    , GetWorkflowRun
    -- * Request Lenses
    , gtwrkflwrnIncludeGraph
    , gtwrkflwrnName
    , gtwrkflwrnRunId

    -- * Destructuring the Response
    , getWorkflowRunResponse
    , GetWorkflowRunResponse
    -- * Response Lenses
    , gtwrkflwrnrsRun
    , gtwrkflwrnrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getWorkflowRun' smart constructor.
data GetWorkflowRun = GetWorkflowRun'{_gtwrkflwrnIncludeGraph
                                      :: !(Maybe Bool),
                                      _gtwrkflwrnName :: !Text,
                                      _gtwrkflwrnRunId :: !Text}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetWorkflowRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtwrkflwrnIncludeGraph' - Specifies whether to include the workflow graph in response or not.
--
-- * 'gtwrkflwrnName' - Name of the workflow being run.
--
-- * 'gtwrkflwrnRunId' - The ID of the workflow run.
getWorkflowRun
    :: Text -- ^ 'gtwrkflwrnName'
    -> Text -- ^ 'gtwrkflwrnRunId'
    -> GetWorkflowRun
getWorkflowRun pName_ pRunId_
  = GetWorkflowRun'{_gtwrkflwrnIncludeGraph = Nothing,
                    _gtwrkflwrnName = pName_, _gtwrkflwrnRunId = pRunId_}

-- | Specifies whether to include the workflow graph in response or not.
gtwrkflwrnIncludeGraph :: Lens' GetWorkflowRun (Maybe Bool)
gtwrkflwrnIncludeGraph = lens _gtwrkflwrnIncludeGraph (\ s a -> s{_gtwrkflwrnIncludeGraph = a})

-- | Name of the workflow being run.
gtwrkflwrnName :: Lens' GetWorkflowRun Text
gtwrkflwrnName = lens _gtwrkflwrnName (\ s a -> s{_gtwrkflwrnName = a})

-- | The ID of the workflow run.
gtwrkflwrnRunId :: Lens' GetWorkflowRun Text
gtwrkflwrnRunId = lens _gtwrkflwrnRunId (\ s a -> s{_gtwrkflwrnRunId = a})

instance AWSRequest GetWorkflowRun where
        type Rs GetWorkflowRun = GetWorkflowRunResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 GetWorkflowRunResponse' <$>
                   (x .?> "Run") <*> (pure (fromEnum s)))

instance Hashable GetWorkflowRun where

instance NFData GetWorkflowRun where

instance ToHeaders GetWorkflowRun where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.GetWorkflowRun" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetWorkflowRun where
        toJSON GetWorkflowRun'{..}
          = object
              (catMaybes
                 [("IncludeGraph" .=) <$> _gtwrkflwrnIncludeGraph,
                  Just ("Name" .= _gtwrkflwrnName),
                  Just ("RunId" .= _gtwrkflwrnRunId)])

instance ToPath GetWorkflowRun where
        toPath = const "/"

instance ToQuery GetWorkflowRun where
        toQuery = const mempty

-- | /See:/ 'getWorkflowRunResponse' smart constructor.
data GetWorkflowRunResponse = GetWorkflowRunResponse'{_gtwrkflwrnrsRun
                                                      :: !(Maybe WorkflowRun),
                                                      _gtwrkflwrnrsResponseStatus
                                                      :: !Int}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'GetWorkflowRunResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtwrkflwrnrsRun' - The requested workflow run metadata.
--
-- * 'gtwrkflwrnrsResponseStatus' - -- | The response status code.
getWorkflowRunResponse
    :: Int -- ^ 'gtwrkflwrnrsResponseStatus'
    -> GetWorkflowRunResponse
getWorkflowRunResponse pResponseStatus_
  = GetWorkflowRunResponse'{_gtwrkflwrnrsRun = Nothing,
                            _gtwrkflwrnrsResponseStatus = pResponseStatus_}

-- | The requested workflow run metadata.
gtwrkflwrnrsRun :: Lens' GetWorkflowRunResponse (Maybe WorkflowRun)
gtwrkflwrnrsRun = lens _gtwrkflwrnrsRun (\ s a -> s{_gtwrkflwrnrsRun = a})

-- | -- | The response status code.
gtwrkflwrnrsResponseStatus :: Lens' GetWorkflowRunResponse Int
gtwrkflwrnrsResponseStatus = lens _gtwrkflwrnrsResponseStatus (\ s a -> s{_gtwrkflwrnrsResponseStatus = a})

instance NFData GetWorkflowRunResponse where
