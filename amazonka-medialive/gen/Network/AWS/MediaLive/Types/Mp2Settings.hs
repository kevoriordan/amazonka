{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Mp2Settings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Mp2Settings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.Mp2CodingMode
import Network.AWS.Prelude

-- | Mp2 Settings
--
-- /See:/ 'mp2Settings' smart constructor.
data Mp2Settings = Mp2Settings'{_mCodingMode ::
                                !(Maybe Mp2CodingMode),
                                _mSampleRate :: !(Maybe Double),
                                _mBitrate :: !(Maybe Double)}
                     deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Mp2Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mCodingMode' - The MPEG2 Audio coding mode.  Valid values are codingMode10 (for mono) or codingMode20 (for stereo).
--
-- * 'mSampleRate' - Sample rate in Hz.
--
-- * 'mBitrate' - Average bitrate in bits/second.
mp2Settings
    :: Mp2Settings
mp2Settings
  = Mp2Settings'{_mCodingMode = Nothing,
                 _mSampleRate = Nothing, _mBitrate = Nothing}

-- | The MPEG2 Audio coding mode.  Valid values are codingMode10 (for mono) or codingMode20 (for stereo).
mCodingMode :: Lens' Mp2Settings (Maybe Mp2CodingMode)
mCodingMode = lens _mCodingMode (\ s a -> s{_mCodingMode = a})

-- | Sample rate in Hz.
mSampleRate :: Lens' Mp2Settings (Maybe Double)
mSampleRate = lens _mSampleRate (\ s a -> s{_mSampleRate = a})

-- | Average bitrate in bits/second.
mBitrate :: Lens' Mp2Settings (Maybe Double)
mBitrate = lens _mBitrate (\ s a -> s{_mBitrate = a})

instance FromJSON Mp2Settings where
        parseJSON
          = withObject "Mp2Settings"
              (\ x ->
                 Mp2Settings' <$>
                   (x .:? "codingMode") <*> (x .:? "sampleRate") <*>
                     (x .:? "bitrate"))

instance Hashable Mp2Settings where

instance NFData Mp2Settings where

instance ToJSON Mp2Settings where
        toJSON Mp2Settings'{..}
          = object
              (catMaybes
                 [("codingMode" .=) <$> _mCodingMode,
                  ("sampleRate" .=) <$> _mSampleRate,
                  ("bitrate" .=) <$> _mBitrate])
