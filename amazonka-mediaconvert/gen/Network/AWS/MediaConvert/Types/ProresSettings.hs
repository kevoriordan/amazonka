{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ProresSettings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.ProresSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.ProresCodecProfile
import Network.AWS.MediaConvert.Types.ProresFramerateControl
import Network.AWS.MediaConvert.Types.ProresFramerateConversionAlgorithm
import Network.AWS.MediaConvert.Types.ProresInterlaceMode
import Network.AWS.MediaConvert.Types.ProresParControl
import Network.AWS.MediaConvert.Types.ProresSlowPal
import Network.AWS.MediaConvert.Types.ProresTelecine
import Network.AWS.Prelude

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value PRORES.
--
-- /See:/ 'proresSettings' smart constructor.
data ProresSettings = ProresSettings'{_psSlowPal ::
                                      !(Maybe ProresSlowPal),
                                      _psParNumerator :: !(Maybe Nat),
                                      _psTelecine :: !(Maybe ProresTelecine),
                                      _psInterlaceMode ::
                                      !(Maybe ProresInterlaceMode),
                                      _psParControl ::
                                      !(Maybe ProresParControl),
                                      _psCodecProfile ::
                                      !(Maybe ProresCodecProfile),
                                      _psFramerateDenominator :: !(Maybe Nat),
                                      _psFramerateConversionAlgorithm ::
                                      !(Maybe
                                          ProresFramerateConversionAlgorithm),
                                      _psFramerateControl ::
                                      !(Maybe ProresFramerateControl),
                                      _psFramerateNumerator :: !(Maybe Nat),
                                      _psParDenominator :: !(Maybe Nat)}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProresSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psSlowPal' - Enables Slow PAL rate conversion. 23.976fps and 24fps input is relabeled as 25fps, and audio is sped up correspondingly.
--
-- * 'psParNumerator' - Pixel Aspect Ratio numerator.
--
-- * 'psTelecine' - Only use Telecine (ProresTelecine) when you set Framerate (Framerate) to 29.970. Set Telecine (ProresTelecine) to Hard (hard) to produce a 29.97i output from a 23.976 input. Set it to Soft (soft) to produce 23.976 output and leave converstion to the player.
--
-- * 'psInterlaceMode' - Use Interlace mode (InterlaceMode) to choose the scan line type for the output. * Top Field First (TOP_FIELD) and Bottom Field First (BOTTOM_FIELD) produce interlaced output with the entire output having the same field polarity (top or bottom first). * Follow, Default Top (FOLLOW_TOP_FIELD) and Follow, Default Bottom (FOLLOW_BOTTOM_FIELD) use the same field polarity as the source. Therefore, behavior depends on the input scan type.   - If the source is interlaced, the output will be interlaced with the same polarity as the source (it will follow the source). The output could therefore be a mix of "top field first" and "bottom field first".   - If the source is progressive, the output will be interlaced with "top field first" or "bottom field first" polarity, depending on which of the Follow options you chose.
--
-- * 'psParControl' - Use (ProresParControl) to specify how the service determines the pixel aspect ratio. Set to Follow source (INITIALIZE_FROM_SOURCE) to use the pixel aspect ratio from the input.  To specify a different pixel aspect ratio: Using the console, choose it from the dropdown menu. Using the API, set ProresParControl to (SPECIFIED) and provide  for (ParNumerator) and (ParDenominator).
--
-- * 'psCodecProfile' - Use Profile (ProResCodecProfile) to specifiy the type of Apple ProRes codec to use for this output.
--
-- * 'psFramerateDenominator' - Frame rate denominator.
--
-- * 'psFramerateConversionAlgorithm' - When set to INTERPOLATE, produces smoother motion during frame rate conversion.
--
-- * 'psFramerateControl' - If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job sepecification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
--
-- * 'psFramerateNumerator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator.
--
-- * 'psParDenominator' - Pixel Aspect Ratio denominator.
proresSettings
    :: ProresSettings
proresSettings
  = ProresSettings'{_psSlowPal = Nothing,
                    _psParNumerator = Nothing, _psTelecine = Nothing,
                    _psInterlaceMode = Nothing, _psParControl = Nothing,
                    _psCodecProfile = Nothing,
                    _psFramerateDenominator = Nothing,
                    _psFramerateConversionAlgorithm = Nothing,
                    _psFramerateControl = Nothing,
                    _psFramerateNumerator = Nothing,
                    _psParDenominator = Nothing}

-- | Enables Slow PAL rate conversion. 23.976fps and 24fps input is relabeled as 25fps, and audio is sped up correspondingly.
psSlowPal :: Lens' ProresSettings (Maybe ProresSlowPal)
psSlowPal = lens _psSlowPal (\ s a -> s{_psSlowPal = a})

-- | Pixel Aspect Ratio numerator.
psParNumerator :: Lens' ProresSettings (Maybe Natural)
psParNumerator = lens _psParNumerator (\ s a -> s{_psParNumerator = a}) . mapping _Nat

-- | Only use Telecine (ProresTelecine) when you set Framerate (Framerate) to 29.970. Set Telecine (ProresTelecine) to Hard (hard) to produce a 29.97i output from a 23.976 input. Set it to Soft (soft) to produce 23.976 output and leave converstion to the player.
psTelecine :: Lens' ProresSettings (Maybe ProresTelecine)
psTelecine = lens _psTelecine (\ s a -> s{_psTelecine = a})

-- | Use Interlace mode (InterlaceMode) to choose the scan line type for the output. * Top Field First (TOP_FIELD) and Bottom Field First (BOTTOM_FIELD) produce interlaced output with the entire output having the same field polarity (top or bottom first). * Follow, Default Top (FOLLOW_TOP_FIELD) and Follow, Default Bottom (FOLLOW_BOTTOM_FIELD) use the same field polarity as the source. Therefore, behavior depends on the input scan type.   - If the source is interlaced, the output will be interlaced with the same polarity as the source (it will follow the source). The output could therefore be a mix of "top field first" and "bottom field first".   - If the source is progressive, the output will be interlaced with "top field first" or "bottom field first" polarity, depending on which of the Follow options you chose.
psInterlaceMode :: Lens' ProresSettings (Maybe ProresInterlaceMode)
psInterlaceMode = lens _psInterlaceMode (\ s a -> s{_psInterlaceMode = a})

-- | Use (ProresParControl) to specify how the service determines the pixel aspect ratio. Set to Follow source (INITIALIZE_FROM_SOURCE) to use the pixel aspect ratio from the input.  To specify a different pixel aspect ratio: Using the console, choose it from the dropdown menu. Using the API, set ProresParControl to (SPECIFIED) and provide  for (ParNumerator) and (ParDenominator).
psParControl :: Lens' ProresSettings (Maybe ProresParControl)
psParControl = lens _psParControl (\ s a -> s{_psParControl = a})

-- | Use Profile (ProResCodecProfile) to specifiy the type of Apple ProRes codec to use for this output.
psCodecProfile :: Lens' ProresSettings (Maybe ProresCodecProfile)
psCodecProfile = lens _psCodecProfile (\ s a -> s{_psCodecProfile = a})

-- | Frame rate denominator.
psFramerateDenominator :: Lens' ProresSettings (Maybe Natural)
psFramerateDenominator = lens _psFramerateDenominator (\ s a -> s{_psFramerateDenominator = a}) . mapping _Nat

-- | When set to INTERPOLATE, produces smoother motion during frame rate conversion.
psFramerateConversionAlgorithm :: Lens' ProresSettings (Maybe ProresFramerateConversionAlgorithm)
psFramerateConversionAlgorithm = lens _psFramerateConversionAlgorithm (\ s a -> s{_psFramerateConversionAlgorithm = a})

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job sepecification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
psFramerateControl :: Lens' ProresSettings (Maybe ProresFramerateControl)
psFramerateControl = lens _psFramerateControl (\ s a -> s{_psFramerateControl = a})

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator.
psFramerateNumerator :: Lens' ProresSettings (Maybe Natural)
psFramerateNumerator = lens _psFramerateNumerator (\ s a -> s{_psFramerateNumerator = a}) . mapping _Nat

-- | Pixel Aspect Ratio denominator.
psParDenominator :: Lens' ProresSettings (Maybe Natural)
psParDenominator = lens _psParDenominator (\ s a -> s{_psParDenominator = a}) . mapping _Nat

instance FromJSON ProresSettings where
        parseJSON
          = withObject "ProresSettings"
              (\ x ->
                 ProresSettings' <$>
                   (x .:? "slowPal") <*> (x .:? "parNumerator") <*>
                     (x .:? "telecine")
                     <*> (x .:? "interlaceMode")
                     <*> (x .:? "parControl")
                     <*> (x .:? "codecProfile")
                     <*> (x .:? "framerateDenominator")
                     <*> (x .:? "framerateConversionAlgorithm")
                     <*> (x .:? "framerateControl")
                     <*> (x .:? "framerateNumerator")
                     <*> (x .:? "parDenominator"))

instance Hashable ProresSettings where

instance NFData ProresSettings where

instance ToJSON ProresSettings where
        toJSON ProresSettings'{..}
          = object
              (catMaybes
                 [("slowPal" .=) <$> _psSlowPal,
                  ("parNumerator" .=) <$> _psParNumerator,
                  ("telecine" .=) <$> _psTelecine,
                  ("interlaceMode" .=) <$> _psInterlaceMode,
                  ("parControl" .=) <$> _psParControl,
                  ("codecProfile" .=) <$> _psCodecProfile,
                  ("framerateDenominator" .=) <$>
                    _psFramerateDenominator,
                  ("framerateConversionAlgorithm" .=) <$>
                    _psFramerateConversionAlgorithm,
                  ("framerateControl" .=) <$> _psFramerateControl,
                  ("framerateNumerator" .=) <$> _psFramerateNumerator,
                  ("parDenominator" .=) <$> _psParDenominator])
