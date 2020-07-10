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
-- Module      : Network.AWS.IoT.DeleteJobExecution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a job execution.
--
--
module Network.AWS.IoT.DeleteJobExecution
    (
    -- * Creating a Request
      deleteJobExecution
    , DeleteJobExecution
    -- * Request Lenses
    , djeForce
    , djeJobId
    , djeThingName
    , djeExecutionNumber

    -- * Destructuring the Response
    , deleteJobExecutionResponse
    , DeleteJobExecutionResponse
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteJobExecution' smart constructor.
data DeleteJobExecution = DeleteJobExecution'{_djeForce
                                              :: !(Maybe Bool),
                                              _djeJobId :: !Text,
                                              _djeThingName :: !Text,
                                              _djeExecutionNumber :: !Integer}
                            deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteJobExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'djeForce' - (Optional) When true, you can delete a job execution which is "IN_PROGRESS". Otherwise, you can only delete a job execution which is in a terminal state ("SUCCEEDED", "FAILED", "REJECTED", "REMOVED" or "CANCELED") or an exception will occur. The default is false.
--
-- * 'djeJobId' - The ID of the job whose execution on a particular device will be deleted.
--
-- * 'djeThingName' - The name of the thing whose job execution will be deleted.
--
-- * 'djeExecutionNumber' - The ID of the job execution to be deleted. The @executionNumber@ refers to the execution of a particular job on a particular device. Note that once a job execution is deleted, the @executionNumber@ may be reused by IoT, so be sure you get and use the correct value here.
deleteJobExecution
    :: Text -- ^ 'djeJobId'
    -> Text -- ^ 'djeThingName'
    -> Integer -- ^ 'djeExecutionNumber'
    -> DeleteJobExecution
deleteJobExecution pJobId_ pThingName_
  pExecutionNumber_
  = DeleteJobExecution'{_djeForce = Nothing,
                        _djeJobId = pJobId_, _djeThingName = pThingName_,
                        _djeExecutionNumber = pExecutionNumber_}

-- | (Optional) When true, you can delete a job execution which is "IN_PROGRESS". Otherwise, you can only delete a job execution which is in a terminal state ("SUCCEEDED", "FAILED", "REJECTED", "REMOVED" or "CANCELED") or an exception will occur. The default is false.
djeForce :: Lens' DeleteJobExecution (Maybe Bool)
djeForce = lens _djeForce (\ s a -> s{_djeForce = a})

-- | The ID of the job whose execution on a particular device will be deleted.
djeJobId :: Lens' DeleteJobExecution Text
djeJobId = lens _djeJobId (\ s a -> s{_djeJobId = a})

-- | The name of the thing whose job execution will be deleted.
djeThingName :: Lens' DeleteJobExecution Text
djeThingName = lens _djeThingName (\ s a -> s{_djeThingName = a})

-- | The ID of the job execution to be deleted. The @executionNumber@ refers to the execution of a particular job on a particular device. Note that once a job execution is deleted, the @executionNumber@ may be reused by IoT, so be sure you get and use the correct value here.
djeExecutionNumber :: Lens' DeleteJobExecution Integer
djeExecutionNumber = lens _djeExecutionNumber (\ s a -> s{_djeExecutionNumber = a})

instance AWSRequest DeleteJobExecution where
        type Rs DeleteJobExecution =
             DeleteJobExecutionResponse
        request = delete ioT
        response = receiveNull DeleteJobExecutionResponse'

instance Hashable DeleteJobExecution where

instance NFData DeleteJobExecution where

instance ToHeaders DeleteJobExecution where
        toHeaders = const mempty

instance ToPath DeleteJobExecution where
        toPath DeleteJobExecution'{..}
          = mconcat
              ["/things/", toBS _djeThingName, "/jobs/",
               toBS _djeJobId, "/executionNumber/",
               toBS _djeExecutionNumber]

instance ToQuery DeleteJobExecution where
        toQuery DeleteJobExecution'{..}
          = mconcat ["force" =: _djeForce]

-- | /See:/ 'deleteJobExecutionResponse' smart constructor.
data DeleteJobExecutionResponse = DeleteJobExecutionResponse'
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'DeleteJobExecutionResponse' with the minimum fields required to make a request.
--
deleteJobExecutionResponse
    :: DeleteJobExecutionResponse
deleteJobExecutionResponse
  = DeleteJobExecutionResponse'

instance NFData DeleteJobExecutionResponse where
