{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NoiseReducerTemporalFilterSettings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.NoiseReducerTemporalFilterSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Noise reducer filter settings for temporal filter.
--
-- /See:/ 'noiseReducerTemporalFilterSettings' smart constructor.
data NoiseReducerTemporalFilterSettings = NoiseReducerTemporalFilterSettings'{_nrtfsAggressiveMode
                                                                              ::
                                                                              !(Maybe
                                                                                  Nat),
                                                                              _nrtfsStrength
                                                                              ::
                                                                              !(Maybe
                                                                                  Nat),
                                                                              _nrtfsSpeed
                                                                              ::
                                                                              !(Maybe
                                                                                  Int)}
                                            deriving (Eq, Read, Show, Data,
                                                      Typeable, Generic)

-- | Creates a value of 'NoiseReducerTemporalFilterSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nrtfsAggressiveMode' - Use Aggressive mode for content that has complex motion. Higher values produce stronger temporal filtering. This filters highly complex scenes more aggressively and creates better VQ for low bitrate outputs.
--
-- * 'nrtfsStrength' - Specify the strength of the noise reducing filter on this output. Higher values produce stronger filtering. We recommend the following value ranges, depending on the result that you want: * 0-2 for complexity reduction with minimal sharpness loss * 2-8 for complexity reduction with image preservation * 8-16 for a high level of complexity reduction
--
-- * 'nrtfsSpeed' - The speed of the filter (higher number is faster). Low setting reduces bit rate at the cost of transcode time, high setting improves transcode time at the cost of bit rate.
noiseReducerTemporalFilterSettings
    :: NoiseReducerTemporalFilterSettings
noiseReducerTemporalFilterSettings
  = NoiseReducerTemporalFilterSettings'{_nrtfsAggressiveMode
                                          = Nothing,
                                        _nrtfsStrength = Nothing,
                                        _nrtfsSpeed = Nothing}

-- | Use Aggressive mode for content that has complex motion. Higher values produce stronger temporal filtering. This filters highly complex scenes more aggressively and creates better VQ for low bitrate outputs.
nrtfsAggressiveMode :: Lens' NoiseReducerTemporalFilterSettings (Maybe Natural)
nrtfsAggressiveMode = lens _nrtfsAggressiveMode (\ s a -> s{_nrtfsAggressiveMode = a}) . mapping _Nat

-- | Specify the strength of the noise reducing filter on this output. Higher values produce stronger filtering. We recommend the following value ranges, depending on the result that you want: * 0-2 for complexity reduction with minimal sharpness loss * 2-8 for complexity reduction with image preservation * 8-16 for a high level of complexity reduction
nrtfsStrength :: Lens' NoiseReducerTemporalFilterSettings (Maybe Natural)
nrtfsStrength = lens _nrtfsStrength (\ s a -> s{_nrtfsStrength = a}) . mapping _Nat

-- | The speed of the filter (higher number is faster). Low setting reduces bit rate at the cost of transcode time, high setting improves transcode time at the cost of bit rate.
nrtfsSpeed :: Lens' NoiseReducerTemporalFilterSettings (Maybe Int)
nrtfsSpeed = lens _nrtfsSpeed (\ s a -> s{_nrtfsSpeed = a})

instance FromJSON NoiseReducerTemporalFilterSettings
         where
        parseJSON
          = withObject "NoiseReducerTemporalFilterSettings"
              (\ x ->
                 NoiseReducerTemporalFilterSettings' <$>
                   (x .:? "aggressiveMode") <*> (x .:? "strength") <*>
                     (x .:? "speed"))

instance Hashable NoiseReducerTemporalFilterSettings
         where

instance NFData NoiseReducerTemporalFilterSettings
         where

instance ToJSON NoiseReducerTemporalFilterSettings
         where
        toJSON NoiseReducerTemporalFilterSettings'{..}
          = object
              (catMaybes
                 [("aggressiveMode" .=) <$> _nrtfsAggressiveMode,
                  ("strength" .=) <$> _nrtfsStrength,
                  ("speed" .=) <$> _nrtfsSpeed])
