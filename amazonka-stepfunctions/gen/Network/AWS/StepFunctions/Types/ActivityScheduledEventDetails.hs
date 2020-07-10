{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ActivityScheduledEventDetails
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.ActivityScheduledEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about an activity scheduled during an execution.
--
--
--
-- /See:/ 'activityScheduledEventDetails' smart constructor.
data ActivityScheduledEventDetails = ActivityScheduledEventDetails'{_asedHeartbeatInSeconds
                                                                    ::
                                                                    !(Maybe
                                                                        Integer),
                                                                    _asedInput
                                                                    ::
                                                                    !(Maybe
                                                                        (Sensitive
                                                                           Text)),
                                                                    _asedTimeoutInSeconds
                                                                    ::
                                                                    !(Maybe
                                                                        Integer),
                                                                    _asedResource
                                                                    :: !Text}
                                       deriving (Eq, Show, Data, Typeable,
                                                 Generic)

-- | Creates a value of 'ActivityScheduledEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asedHeartbeatInSeconds' - The maximum allowed duration between two heartbeats for the activity task.
--
-- * 'asedInput' - The JSON data input to the activity task.
--
-- * 'asedTimeoutInSeconds' - The maximum allowed duration of the activity task.
--
-- * 'asedResource' - The Amazon Resource Name (ARN) of the scheduled activity.
activityScheduledEventDetails
    :: Text -- ^ 'asedResource'
    -> ActivityScheduledEventDetails
activityScheduledEventDetails pResource_
  = ActivityScheduledEventDetails'{_asedHeartbeatInSeconds
                                     = Nothing,
                                   _asedInput = Nothing,
                                   _asedTimeoutInSeconds = Nothing,
                                   _asedResource = pResource_}

-- | The maximum allowed duration between two heartbeats for the activity task.
asedHeartbeatInSeconds :: Lens' ActivityScheduledEventDetails (Maybe Integer)
asedHeartbeatInSeconds = lens _asedHeartbeatInSeconds (\ s a -> s{_asedHeartbeatInSeconds = a})

-- | The JSON data input to the activity task.
asedInput :: Lens' ActivityScheduledEventDetails (Maybe Text)
asedInput = lens _asedInput (\ s a -> s{_asedInput = a}) . mapping _Sensitive

-- | The maximum allowed duration of the activity task.
asedTimeoutInSeconds :: Lens' ActivityScheduledEventDetails (Maybe Integer)
asedTimeoutInSeconds = lens _asedTimeoutInSeconds (\ s a -> s{_asedTimeoutInSeconds = a})

-- | The Amazon Resource Name (ARN) of the scheduled activity.
asedResource :: Lens' ActivityScheduledEventDetails Text
asedResource = lens _asedResource (\ s a -> s{_asedResource = a})

instance FromJSON ActivityScheduledEventDetails where
        parseJSON
          = withObject "ActivityScheduledEventDetails"
              (\ x ->
                 ActivityScheduledEventDetails' <$>
                   (x .:? "heartbeatInSeconds") <*> (x .:? "input") <*>
                     (x .:? "timeoutInSeconds")
                     <*> (x .: "resource"))

instance Hashable ActivityScheduledEventDetails where

instance NFData ActivityScheduledEventDetails where
