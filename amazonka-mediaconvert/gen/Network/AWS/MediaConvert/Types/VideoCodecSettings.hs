{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.VideoCodecSettings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.VideoCodecSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.Av1Settings
import Network.AWS.MediaConvert.Types.FrameCaptureSettings
import Network.AWS.MediaConvert.Types.H264Settings
import Network.AWS.MediaConvert.Types.H265Settings
import Network.AWS.MediaConvert.Types.Mpeg2Settings
import Network.AWS.MediaConvert.Types.ProresSettings
import Network.AWS.MediaConvert.Types.VideoCodec
import Network.AWS.Prelude

-- | Video codec settings, (CodecSettings) under (VideoDescription), contains the group of settings related to video encoding. The settings in this group vary depending on the value that you choose for Video codec (Codec). For each codec enum that you choose, define the corresponding settings object. The following lists the codec enum, settings object pairs. * FRAME_CAPTURE, FrameCaptureSettings * AV1, Av1Settings * H_264, H264Settings * H_265, H265Settings * MPEG2, Mpeg2Settings * PRORES, ProresSettings
--
-- /See:/ 'videoCodecSettings' smart constructor.
data VideoCodecSettings = VideoCodecSettings'{_vcsFrameCaptureSettings
                                              :: !(Maybe FrameCaptureSettings),
                                              _vcsAv1Settings ::
                                              !(Maybe Av1Settings),
                                              _vcsCodec :: !(Maybe VideoCodec),
                                              _vcsH265Settings ::
                                              !(Maybe H265Settings),
                                              _vcsProresSettings ::
                                              !(Maybe ProresSettings),
                                              _vcsH264Settings ::
                                              !(Maybe H264Settings),
                                              _vcsMpeg2Settings ::
                                              !(Maybe Mpeg2Settings)}
                            deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VideoCodecSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcsFrameCaptureSettings' - Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value FRAME_CAPTURE.
--
-- * 'vcsAv1Settings' - Required when you set Codec, under VideoDescription>CodecSettings to the value AV1.
--
-- * 'vcsCodec' - Specifies the video codec. This must be equal to one of the enum values defined by the object  VideoCodec.
--
-- * 'vcsH265Settings' - Settings for H265 codec
--
-- * 'vcsProresSettings' - Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value PRORES.
--
-- * 'vcsH264Settings' - Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value H_264.
--
-- * 'vcsMpeg2Settings' - Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value MPEG2.
videoCodecSettings
    :: VideoCodecSettings
videoCodecSettings
  = VideoCodecSettings'{_vcsFrameCaptureSettings =
                          Nothing,
                        _vcsAv1Settings = Nothing, _vcsCodec = Nothing,
                        _vcsH265Settings = Nothing,
                        _vcsProresSettings = Nothing,
                        _vcsH264Settings = Nothing,
                        _vcsMpeg2Settings = Nothing}

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value FRAME_CAPTURE.
vcsFrameCaptureSettings :: Lens' VideoCodecSettings (Maybe FrameCaptureSettings)
vcsFrameCaptureSettings = lens _vcsFrameCaptureSettings (\ s a -> s{_vcsFrameCaptureSettings = a})

-- | Required when you set Codec, under VideoDescription>CodecSettings to the value AV1.
vcsAv1Settings :: Lens' VideoCodecSettings (Maybe Av1Settings)
vcsAv1Settings = lens _vcsAv1Settings (\ s a -> s{_vcsAv1Settings = a})

-- | Specifies the video codec. This must be equal to one of the enum values defined by the object  VideoCodec.
vcsCodec :: Lens' VideoCodecSettings (Maybe VideoCodec)
vcsCodec = lens _vcsCodec (\ s a -> s{_vcsCodec = a})

-- | Settings for H265 codec
vcsH265Settings :: Lens' VideoCodecSettings (Maybe H265Settings)
vcsH265Settings = lens _vcsH265Settings (\ s a -> s{_vcsH265Settings = a})

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value PRORES.
vcsProresSettings :: Lens' VideoCodecSettings (Maybe ProresSettings)
vcsProresSettings = lens _vcsProresSettings (\ s a -> s{_vcsProresSettings = a})

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value H_264.
vcsH264Settings :: Lens' VideoCodecSettings (Maybe H264Settings)
vcsH264Settings = lens _vcsH264Settings (\ s a -> s{_vcsH264Settings = a})

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value MPEG2.
vcsMpeg2Settings :: Lens' VideoCodecSettings (Maybe Mpeg2Settings)
vcsMpeg2Settings = lens _vcsMpeg2Settings (\ s a -> s{_vcsMpeg2Settings = a})

instance FromJSON VideoCodecSettings where
        parseJSON
          = withObject "VideoCodecSettings"
              (\ x ->
                 VideoCodecSettings' <$>
                   (x .:? "frameCaptureSettings") <*>
                     (x .:? "av1Settings")
                     <*> (x .:? "codec")
                     <*> (x .:? "h265Settings")
                     <*> (x .:? "proresSettings")
                     <*> (x .:? "h264Settings")
                     <*> (x .:? "mpeg2Settings"))

instance Hashable VideoCodecSettings where

instance NFData VideoCodecSettings where

instance ToJSON VideoCodecSettings where
        toJSON VideoCodecSettings'{..}
          = object
              (catMaybes
                 [("frameCaptureSettings" .=) <$>
                    _vcsFrameCaptureSettings,
                  ("av1Settings" .=) <$> _vcsAv1Settings,
                  ("codec" .=) <$> _vcsCodec,
                  ("h265Settings" .=) <$> _vcsH265Settings,
                  ("proresSettings" .=) <$> _vcsProresSettings,
                  ("h264Settings" .=) <$> _vcsH264Settings,
                  ("mpeg2Settings" .=) <$> _vcsMpeg2Settings])
