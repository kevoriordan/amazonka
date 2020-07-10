{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.TestingDataResult
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.TestingDataResult where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.TestingData

-- | A Sagemaker Groundtruth format manifest file representing the dataset used for testing.
--
--
--
-- /See:/ 'testingDataResult' smart constructor.
data TestingDataResult = TestingDataResult'{_tdrInput
                                            :: !(Maybe TestingData),
                                            _tdrOutput :: !(Maybe TestingData)}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TestingDataResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdrInput' - The testing dataset that was supplied for training.
--
-- * 'tdrOutput' - The subset of the dataset that was actually tested. Some images (assets) might not be tested due to file formatting and other issues. 
testingDataResult
    :: TestingDataResult
testingDataResult
  = TestingDataResult'{_tdrInput = Nothing,
                       _tdrOutput = Nothing}

-- | The testing dataset that was supplied for training.
tdrInput :: Lens' TestingDataResult (Maybe TestingData)
tdrInput = lens _tdrInput (\ s a -> s{_tdrInput = a})

-- | The subset of the dataset that was actually tested. Some images (assets) might not be tested due to file formatting and other issues. 
tdrOutput :: Lens' TestingDataResult (Maybe TestingData)
tdrOutput = lens _tdrOutput (\ s a -> s{_tdrOutput = a})

instance FromJSON TestingDataResult where
        parseJSON
          = withObject "TestingDataResult"
              (\ x ->
                 TestingDataResult' <$>
                   (x .:? "Input") <*> (x .:? "Output"))

instance Hashable TestingDataResult where

instance NFData TestingDataResult where
