{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MxfSettings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.MxfSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.MxfAfdSignaling
import Network.AWS.Prelude

-- | MXF settings
--
-- /See:/ 'mxfSettings' smart constructor.
newtype MxfSettings = MxfSettings'{_msAfdSignaling ::
                                   Maybe MxfAfdSignaling}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MxfSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msAfdSignaling' - Optional. When you have AFD signaling set up in your output video stream, use this setting to choose whether to also include it in the MXF wrapper. Choose Don't copy (NO_COPY) to exclude AFD signaling from the MXF wrapper. Choose Copy from video stream (COPY_FROM_VIDEO) to copy the AFD values from the video stream for this output to the MXF wrapper. Regardless of which option you choose, the AFD values remain in the video stream. Related settings: To set up your output to include or exclude AFD values, see AfdSignaling, under VideoDescription. On the console, find AFD signaling under the output's video encoding settings.
mxfSettings
    :: MxfSettings
mxfSettings = MxfSettings'{_msAfdSignaling = Nothing}

-- | Optional. When you have AFD signaling set up in your output video stream, use this setting to choose whether to also include it in the MXF wrapper. Choose Don't copy (NO_COPY) to exclude AFD signaling from the MXF wrapper. Choose Copy from video stream (COPY_FROM_VIDEO) to copy the AFD values from the video stream for this output to the MXF wrapper. Regardless of which option you choose, the AFD values remain in the video stream. Related settings: To set up your output to include or exclude AFD values, see AfdSignaling, under VideoDescription. On the console, find AFD signaling under the output's video encoding settings.
msAfdSignaling :: Lens' MxfSettings (Maybe MxfAfdSignaling)
msAfdSignaling = lens _msAfdSignaling (\ s a -> s{_msAfdSignaling = a})

instance FromJSON MxfSettings where
        parseJSON
          = withObject "MxfSettings"
              (\ x -> MxfSettings' <$> (x .:? "afdSignaling"))

instance Hashable MxfSettings where

instance NFData MxfSettings where

instance ToJSON MxfSettings where
        toJSON MxfSettings'{..}
          = object
              (catMaybes [("afdSignaling" .=) <$> _msAfdSignaling])
