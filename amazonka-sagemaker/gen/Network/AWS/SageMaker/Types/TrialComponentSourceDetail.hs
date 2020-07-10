{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialComponentSourceDetail
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.TrialComponentSourceDetail where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.ProcessingJob
import Network.AWS.SageMaker.Types.TrainingJob

-- | Detailed information about the source of a trial component. Either @ProcessingJob@ or @TrainingJob@ is returned.
--
--
--
-- /See:/ 'trialComponentSourceDetail' smart constructor.
data TrialComponentSourceDetail = TrialComponentSourceDetail'{_tcsdTrainingJob
                                                              ::
                                                              !(Maybe
                                                                  TrainingJob),
                                                              _tcsdSourceARN ::
                                                              !(Maybe Text),
                                                              _tcsdProcessingJob
                                                              ::
                                                              !(Maybe
                                                                  ProcessingJob)}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'TrialComponentSourceDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcsdTrainingJob' - Information about a training job that's the source of a trial component.
--
-- * 'tcsdSourceARN' - The Amazon Resource Name (ARN) of the source.
--
-- * 'tcsdProcessingJob' - Information about a processing job that's the source of a trial component.
trialComponentSourceDetail
    :: TrialComponentSourceDetail
trialComponentSourceDetail
  = TrialComponentSourceDetail'{_tcsdTrainingJob =
                                  Nothing,
                                _tcsdSourceARN = Nothing,
                                _tcsdProcessingJob = Nothing}

-- | Information about a training job that's the source of a trial component.
tcsdTrainingJob :: Lens' TrialComponentSourceDetail (Maybe TrainingJob)
tcsdTrainingJob = lens _tcsdTrainingJob (\ s a -> s{_tcsdTrainingJob = a})

-- | The Amazon Resource Name (ARN) of the source.
tcsdSourceARN :: Lens' TrialComponentSourceDetail (Maybe Text)
tcsdSourceARN = lens _tcsdSourceARN (\ s a -> s{_tcsdSourceARN = a})

-- | Information about a processing job that's the source of a trial component.
tcsdProcessingJob :: Lens' TrialComponentSourceDetail (Maybe ProcessingJob)
tcsdProcessingJob = lens _tcsdProcessingJob (\ s a -> s{_tcsdProcessingJob = a})

instance FromJSON TrialComponentSourceDetail where
        parseJSON
          = withObject "TrialComponentSourceDetail"
              (\ x ->
                 TrialComponentSourceDetail' <$>
                   (x .:? "TrainingJob") <*> (x .:? "SourceArn") <*>
                     (x .:? "ProcessingJob"))

instance Hashable TrialComponentSourceDetail where

instance NFData TrialComponentSourceDetail where
