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
-- Module      : Network.AWS.IoT.UpdateJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates supported fields of the specified job.
--
--
module Network.AWS.IoT.UpdateJob
    (
    -- * Creating a Request
      updateJob
    , UpdateJob
    -- * Request Lenses
    , ujJobExecutionsRolloutConfig
    , ujAbortConfig
    , ujPresignedURLConfig
    , ujDescription
    , ujTimeoutConfig
    , ujJobId

    -- * Destructuring the Response
    , updateJobResponse
    , UpdateJobResponse
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateJob' smart constructor.
data UpdateJob = UpdateJob'{_ujJobExecutionsRolloutConfig
                            :: !(Maybe JobExecutionsRolloutConfig),
                            _ujAbortConfig :: !(Maybe AbortConfig),
                            _ujPresignedURLConfig ::
                            !(Maybe PresignedURLConfig),
                            _ujDescription :: !(Maybe Text),
                            _ujTimeoutConfig :: !(Maybe TimeoutConfig),
                            _ujJobId :: !Text}
                   deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ujJobExecutionsRolloutConfig' - Allows you to create a staged rollout of the job.
--
-- * 'ujAbortConfig' - Allows you to create criteria to abort a job.
--
-- * 'ujPresignedURLConfig' - Configuration information for pre-signed S3 URLs.
--
-- * 'ujDescription' - A short text description of the job.
--
-- * 'ujTimeoutConfig' - Specifies the amount of time each device has to finish its execution of the job. The timer is started when the job execution status is set to @IN_PROGRESS@ . If the job execution status is not set to another terminal state before the time expires, it will be automatically set to @TIMED_OUT@ . 
--
-- * 'ujJobId' - The ID of the job to be updated.
updateJob
    :: Text -- ^ 'ujJobId'
    -> UpdateJob
updateJob pJobId_
  = UpdateJob'{_ujJobExecutionsRolloutConfig = Nothing,
               _ujAbortConfig = Nothing,
               _ujPresignedURLConfig = Nothing,
               _ujDescription = Nothing, _ujTimeoutConfig = Nothing,
               _ujJobId = pJobId_}

-- | Allows you to create a staged rollout of the job.
ujJobExecutionsRolloutConfig :: Lens' UpdateJob (Maybe JobExecutionsRolloutConfig)
ujJobExecutionsRolloutConfig = lens _ujJobExecutionsRolloutConfig (\ s a -> s{_ujJobExecutionsRolloutConfig = a})

-- | Allows you to create criteria to abort a job.
ujAbortConfig :: Lens' UpdateJob (Maybe AbortConfig)
ujAbortConfig = lens _ujAbortConfig (\ s a -> s{_ujAbortConfig = a})

-- | Configuration information for pre-signed S3 URLs.
ujPresignedURLConfig :: Lens' UpdateJob (Maybe PresignedURLConfig)
ujPresignedURLConfig = lens _ujPresignedURLConfig (\ s a -> s{_ujPresignedURLConfig = a})

-- | A short text description of the job.
ujDescription :: Lens' UpdateJob (Maybe Text)
ujDescription = lens _ujDescription (\ s a -> s{_ujDescription = a})

-- | Specifies the amount of time each device has to finish its execution of the job. The timer is started when the job execution status is set to @IN_PROGRESS@ . If the job execution status is not set to another terminal state before the time expires, it will be automatically set to @TIMED_OUT@ . 
ujTimeoutConfig :: Lens' UpdateJob (Maybe TimeoutConfig)
ujTimeoutConfig = lens _ujTimeoutConfig (\ s a -> s{_ujTimeoutConfig = a})

-- | The ID of the job to be updated.
ujJobId :: Lens' UpdateJob Text
ujJobId = lens _ujJobId (\ s a -> s{_ujJobId = a})

instance AWSRequest UpdateJob where
        type Rs UpdateJob = UpdateJobResponse
        request = patchJSON ioT
        response = receiveNull UpdateJobResponse'

instance Hashable UpdateJob where

instance NFData UpdateJob where

instance ToHeaders UpdateJob where
        toHeaders = const mempty

instance ToJSON UpdateJob where
        toJSON UpdateJob'{..}
          = object
              (catMaybes
                 [("jobExecutionsRolloutConfig" .=) <$>
                    _ujJobExecutionsRolloutConfig,
                  ("abortConfig" .=) <$> _ujAbortConfig,
                  ("presignedUrlConfig" .=) <$> _ujPresignedURLConfig,
                  ("description" .=) <$> _ujDescription,
                  ("timeoutConfig" .=) <$> _ujTimeoutConfig])

instance ToPath UpdateJob where
        toPath UpdateJob'{..}
          = mconcat ["/jobs/", toBS _ujJobId]

instance ToQuery UpdateJob where
        toQuery = const mempty

-- | /See:/ 'updateJobResponse' smart constructor.
data UpdateJobResponse = UpdateJobResponse'
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateJobResponse' with the minimum fields required to make a request.
--
updateJobResponse
    :: UpdateJobResponse
updateJobResponse = UpdateJobResponse'

instance NFData UpdateJobResponse where
