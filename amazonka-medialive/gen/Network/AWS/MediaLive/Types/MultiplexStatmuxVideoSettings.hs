{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexStatmuxVideoSettings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.MultiplexStatmuxVideoSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Statmux rate control settings
--
-- /See:/ 'multiplexStatmuxVideoSettings' smart constructor.
data MultiplexStatmuxVideoSettings = MultiplexStatmuxVideoSettings'{_msvsMinimumBitrate
                                                                    ::
                                                                    !(Maybe
                                                                        Nat),
                                                                    _msvsMaximumBitrate
                                                                    ::
                                                                    !(Maybe
                                                                        Nat)}
                                       deriving (Eq, Read, Show, Data, Typeable,
                                                 Generic)

-- | Creates a value of 'MultiplexStatmuxVideoSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msvsMinimumBitrate' - Minimum statmux bitrate.
--
-- * 'msvsMaximumBitrate' - Maximum statmux bitrate.
multiplexStatmuxVideoSettings
    :: MultiplexStatmuxVideoSettings
multiplexStatmuxVideoSettings
  = MultiplexStatmuxVideoSettings'{_msvsMinimumBitrate
                                     = Nothing,
                                   _msvsMaximumBitrate = Nothing}

-- | Minimum statmux bitrate.
msvsMinimumBitrate :: Lens' MultiplexStatmuxVideoSettings (Maybe Natural)
msvsMinimumBitrate = lens _msvsMinimumBitrate (\ s a -> s{_msvsMinimumBitrate = a}) . mapping _Nat

-- | Maximum statmux bitrate.
msvsMaximumBitrate :: Lens' MultiplexStatmuxVideoSettings (Maybe Natural)
msvsMaximumBitrate = lens _msvsMaximumBitrate (\ s a -> s{_msvsMaximumBitrate = a}) . mapping _Nat

instance FromJSON MultiplexStatmuxVideoSettings where
        parseJSON
          = withObject "MultiplexStatmuxVideoSettings"
              (\ x ->
                 MultiplexStatmuxVideoSettings' <$>
                   (x .:? "minimumBitrate") <*>
                     (x .:? "maximumBitrate"))

instance Hashable MultiplexStatmuxVideoSettings where

instance NFData MultiplexStatmuxVideoSettings where

instance ToJSON MultiplexStatmuxVideoSettings where
        toJSON MultiplexStatmuxVideoSettings'{..}
          = object
              (catMaybes
                 [("minimumBitrate" .=) <$> _msvsMinimumBitrate,
                  ("maximumBitrate" .=) <$> _msvsMaximumBitrate])
