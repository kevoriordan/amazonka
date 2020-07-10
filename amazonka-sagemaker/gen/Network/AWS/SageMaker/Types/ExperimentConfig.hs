{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ExperimentConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ExperimentConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration for the experiment.
--
--
--
-- /See:/ 'experimentConfig' smart constructor.
data ExperimentConfig = ExperimentConfig'{_ecTrialComponentDisplayName
                                          :: !(Maybe Text),
                                          _ecExperimentName :: !(Maybe Text),
                                          _ecTrialName :: !(Maybe Text)}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExperimentConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecTrialComponentDisplayName' - Display name for the trial component.
--
-- * 'ecExperimentName' - The name of the experiment.
--
-- * 'ecTrialName' - The name of the trial.
experimentConfig
    :: ExperimentConfig
experimentConfig
  = ExperimentConfig'{_ecTrialComponentDisplayName =
                        Nothing,
                      _ecExperimentName = Nothing, _ecTrialName = Nothing}

-- | Display name for the trial component.
ecTrialComponentDisplayName :: Lens' ExperimentConfig (Maybe Text)
ecTrialComponentDisplayName = lens _ecTrialComponentDisplayName (\ s a -> s{_ecTrialComponentDisplayName = a})

-- | The name of the experiment.
ecExperimentName :: Lens' ExperimentConfig (Maybe Text)
ecExperimentName = lens _ecExperimentName (\ s a -> s{_ecExperimentName = a})

-- | The name of the trial.
ecTrialName :: Lens' ExperimentConfig (Maybe Text)
ecTrialName = lens _ecTrialName (\ s a -> s{_ecTrialName = a})

instance FromJSON ExperimentConfig where
        parseJSON
          = withObject "ExperimentConfig"
              (\ x ->
                 ExperimentConfig' <$>
                   (x .:? "TrialComponentDisplayName") <*>
                     (x .:? "ExperimentName")
                     <*> (x .:? "TrialName"))

instance Hashable ExperimentConfig where

instance NFData ExperimentConfig where

instance ToJSON ExperimentConfig where
        toJSON ExperimentConfig'{..}
          = object
              (catMaybes
                 [("TrialComponentDisplayName" .=) <$>
                    _ecTrialComponentDisplayName,
                  ("ExperimentName" .=) <$> _ecExperimentName,
                  ("TrialName" .=) <$> _ecTrialName])
