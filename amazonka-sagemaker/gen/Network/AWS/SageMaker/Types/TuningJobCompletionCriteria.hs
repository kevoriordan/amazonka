{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TuningJobCompletionCriteria
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.TuningJobCompletionCriteria where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The job completion criteria.
--
--
--
-- /See:/ 'tuningJobCompletionCriteria' smart constructor.
newtype TuningJobCompletionCriteria = TuningJobCompletionCriteria'{_tjccTargetObjectiveMetricValue
                                                                   :: Double}
                                        deriving (Eq, Read, Show, Data,
                                                  Typeable, Generic)

-- | Creates a value of 'TuningJobCompletionCriteria' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tjccTargetObjectiveMetricValue' - The objective metric's value.
tuningJobCompletionCriteria
    :: Double -- ^ 'tjccTargetObjectiveMetricValue'
    -> TuningJobCompletionCriteria
tuningJobCompletionCriteria
  pTargetObjectiveMetricValue_
  = TuningJobCompletionCriteria'{_tjccTargetObjectiveMetricValue
                                   = pTargetObjectiveMetricValue_}

-- | The objective metric's value.
tjccTargetObjectiveMetricValue :: Lens' TuningJobCompletionCriteria Double
tjccTargetObjectiveMetricValue = lens _tjccTargetObjectiveMetricValue (\ s a -> s{_tjccTargetObjectiveMetricValue = a})

instance FromJSON TuningJobCompletionCriteria where
        parseJSON
          = withObject "TuningJobCompletionCriteria"
              (\ x ->
                 TuningJobCompletionCriteria' <$>
                   (x .: "TargetObjectiveMetricValue"))

instance Hashable TuningJobCompletionCriteria where

instance NFData TuningJobCompletionCriteria where

instance ToJSON TuningJobCompletionCriteria where
        toJSON TuningJobCompletionCriteria'{..}
          = object
              (catMaybes
                 [Just
                    ("TargetObjectiveMetricValue" .=
                       _tjccTargetObjectiveMetricValue)])
