{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.StartTimerFailedEventAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.StartTimerFailedEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.StartTimerFailedCause

-- | Provides the details of the @StartTimerFailed@ event.
--
--
--
-- /See:/ 'startTimerFailedEventAttributes' smart constructor.
data StartTimerFailedEventAttributes = StartTimerFailedEventAttributes'{_stfeaTimerId
                                                                        ::
                                                                        !Text,
                                                                        _stfeaCause
                                                                        ::
                                                                        !StartTimerFailedCause,
                                                                        _stfeaDecisionTaskCompletedEventId
                                                                        ::
                                                                        !Integer}
                                         deriving (Eq, Read, Show, Data,
                                                   Typeable, Generic)

-- | Creates a value of 'StartTimerFailedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stfeaTimerId' - The timerId provided in the @StartTimer@ decision that failed.
--
-- * 'stfeaCause' - The cause of the failure. This information is generated by the system and can be useful for diagnostic purposes.
--
-- * 'stfeaDecisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @StartTimer@ decision for this activity task. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
startTimerFailedEventAttributes
    :: Text -- ^ 'stfeaTimerId'
    -> StartTimerFailedCause -- ^ 'stfeaCause'
    -> Integer -- ^ 'stfeaDecisionTaskCompletedEventId'
    -> StartTimerFailedEventAttributes
startTimerFailedEventAttributes pTimerId_ pCause_
  pDecisionTaskCompletedEventId_
  = StartTimerFailedEventAttributes'{_stfeaTimerId =
                                       pTimerId_,
                                     _stfeaCause = pCause_,
                                     _stfeaDecisionTaskCompletedEventId =
                                       pDecisionTaskCompletedEventId_}

-- | The timerId provided in the @StartTimer@ decision that failed.
stfeaTimerId :: Lens' StartTimerFailedEventAttributes Text
stfeaTimerId = lens _stfeaTimerId (\ s a -> s{_stfeaTimerId = a})

-- | The cause of the failure. This information is generated by the system and can be useful for diagnostic purposes.
stfeaCause :: Lens' StartTimerFailedEventAttributes StartTimerFailedCause
stfeaCause = lens _stfeaCause (\ s a -> s{_stfeaCause = a})

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @StartTimer@ decision for this activity task. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
stfeaDecisionTaskCompletedEventId :: Lens' StartTimerFailedEventAttributes Integer
stfeaDecisionTaskCompletedEventId = lens _stfeaDecisionTaskCompletedEventId (\ s a -> s{_stfeaDecisionTaskCompletedEventId = a})

instance FromJSON StartTimerFailedEventAttributes
         where
        parseJSON
          = withObject "StartTimerFailedEventAttributes"
              (\ x ->
                 StartTimerFailedEventAttributes' <$>
                   (x .: "timerId") <*> (x .: "cause") <*>
                     (x .: "decisionTaskCompletedEventId"))

instance Hashable StartTimerFailedEventAttributes
         where

instance NFData StartTimerFailedEventAttributes where
