{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.TrainingDataResult
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.TrainingDataResult where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.TrainingData

-- | A Sagemaker Groundtruth format manifest file that represents the dataset used for training.
--
--
--
-- /See:/ 'trainingDataResult' smart constructor.
data TrainingDataResult = TrainingDataResult'{_tInput
                                              :: !(Maybe TrainingData),
                                              _tOutput :: !(Maybe TrainingData)}
                            deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrainingDataResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tInput' - The training assets that you supplied for training.
--
-- * 'tOutput' - The images (assets) that were actually trained by Amazon Rekognition Custom Labels. 
trainingDataResult
    :: TrainingDataResult
trainingDataResult
  = TrainingDataResult'{_tInput = Nothing,
                        _tOutput = Nothing}

-- | The training assets that you supplied for training.
tInput :: Lens' TrainingDataResult (Maybe TrainingData)
tInput = lens _tInput (\ s a -> s{_tInput = a})

-- | The images (assets) that were actually trained by Amazon Rekognition Custom Labels. 
tOutput :: Lens' TrainingDataResult (Maybe TrainingData)
tOutput = lens _tOutput (\ s a -> s{_tOutput = a})

instance FromJSON TrainingDataResult where
        parseJSON
          = withObject "TrainingDataResult"
              (\ x ->
                 TrainingDataResult' <$>
                   (x .:? "Input") <*> (x .:? "Output"))

instance Hashable TrainingDataResult where

instance NFData TrainingDataResult where
