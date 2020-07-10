{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AbortCriteria
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AbortCriteria where

import Network.AWS.IoT.Types.AbortAction
import Network.AWS.IoT.Types.JobExecutionFailureType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details of abort criteria to define rules to abort the job.
--
--
--
-- /See:/ 'abortCriteria' smart constructor.
data AbortCriteria = AbortCriteria'{_acFailureType ::
                                    !JobExecutionFailureType,
                                    _acAction :: !AbortAction,
                                    _acThresholdPercentage :: !Double,
                                    _acMinNumberOfExecutedThings :: !Nat}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AbortCriteria' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acFailureType' - The type of job execution failure to define a rule to initiate a job abort.
--
-- * 'acAction' - The type of abort action to initiate a job abort.
--
-- * 'acThresholdPercentage' - The threshold as a percentage of the total number of executed things that will initiate a job abort. AWS IoT supports up to two digits after the decimal (for example, 10.9 and 10.99, but not 10.999).
--
-- * 'acMinNumberOfExecutedThings' - Minimum number of executed things before evaluating an abort rule.
abortCriteria
    :: JobExecutionFailureType -- ^ 'acFailureType'
    -> AbortAction -- ^ 'acAction'
    -> Double -- ^ 'acThresholdPercentage'
    -> Natural -- ^ 'acMinNumberOfExecutedThings'
    -> AbortCriteria
abortCriteria pFailureType_ pAction_
  pThresholdPercentage_ pMinNumberOfExecutedThings_
  = AbortCriteria'{_acFailureType = pFailureType_,
                   _acAction = pAction_,
                   _acThresholdPercentage = pThresholdPercentage_,
                   _acMinNumberOfExecutedThings =
                     _Nat # pMinNumberOfExecutedThings_}

-- | The type of job execution failure to define a rule to initiate a job abort.
acFailureType :: Lens' AbortCriteria JobExecutionFailureType
acFailureType = lens _acFailureType (\ s a -> s{_acFailureType = a})

-- | The type of abort action to initiate a job abort.
acAction :: Lens' AbortCriteria AbortAction
acAction = lens _acAction (\ s a -> s{_acAction = a})

-- | The threshold as a percentage of the total number of executed things that will initiate a job abort. AWS IoT supports up to two digits after the decimal (for example, 10.9 and 10.99, but not 10.999).
acThresholdPercentage :: Lens' AbortCriteria Double
acThresholdPercentage = lens _acThresholdPercentage (\ s a -> s{_acThresholdPercentage = a})

-- | Minimum number of executed things before evaluating an abort rule.
acMinNumberOfExecutedThings :: Lens' AbortCriteria Natural
acMinNumberOfExecutedThings = lens _acMinNumberOfExecutedThings (\ s a -> s{_acMinNumberOfExecutedThings = a}) . _Nat

instance FromJSON AbortCriteria where
        parseJSON
          = withObject "AbortCriteria"
              (\ x ->
                 AbortCriteria' <$>
                   (x .: "failureType") <*> (x .: "action") <*>
                     (x .: "thresholdPercentage")
                     <*> (x .: "minNumberOfExecutedThings"))

instance Hashable AbortCriteria where

instance NFData AbortCriteria where

instance ToJSON AbortCriteria where
        toJSON AbortCriteria'{..}
          = object
              (catMaybes
                 [Just ("failureType" .= _acFailureType),
                  Just ("action" .= _acAction),
                  Just
                    ("thresholdPercentage" .= _acThresholdPercentage),
                  Just
                    ("minNumberOfExecutedThings" .=
                       _acMinNumberOfExecutedThings)])
