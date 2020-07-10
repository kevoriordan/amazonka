{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2Settings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Mpeg2Settings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.Mpeg2AdaptiveQuantization
import Network.AWS.MediaConvert.Types.Mpeg2CodecLevel
import Network.AWS.MediaConvert.Types.Mpeg2CodecProfile
import Network.AWS.MediaConvert.Types.Mpeg2DynamicSubGop
import Network.AWS.MediaConvert.Types.Mpeg2FramerateControl
import Network.AWS.MediaConvert.Types.Mpeg2FramerateConversionAlgorithm
import Network.AWS.MediaConvert.Types.Mpeg2GopSizeUnits
import Network.AWS.MediaConvert.Types.Mpeg2InterlaceMode
import Network.AWS.MediaConvert.Types.Mpeg2IntraDcPrecision
import Network.AWS.MediaConvert.Types.Mpeg2ParControl
import Network.AWS.MediaConvert.Types.Mpeg2QualityTuningLevel
import Network.AWS.MediaConvert.Types.Mpeg2RateControlMode
import Network.AWS.MediaConvert.Types.Mpeg2SceneChangeDetect
import Network.AWS.MediaConvert.Types.Mpeg2SlowPal
import Network.AWS.MediaConvert.Types.Mpeg2SpatialAdaptiveQuantization
import Network.AWS.MediaConvert.Types.Mpeg2Syntax
import Network.AWS.MediaConvert.Types.Mpeg2Telecine
import Network.AWS.MediaConvert.Types.Mpeg2TemporalAdaptiveQuantization
import Network.AWS.Prelude

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value MPEG2.
--
-- /See:/ 'mpeg2Settings' smart constructor.
data Mpeg2Settings = Mpeg2Settings'{_msQualityTuningLevel
                                    :: !(Maybe Mpeg2QualityTuningLevel),
                                    _msTemporalAdaptiveQuantization ::
                                    !(Maybe Mpeg2TemporalAdaptiveQuantization),
                                    _msSceneChangeDetect ::
                                    !(Maybe Mpeg2SceneChangeDetect),
                                    _msHrdBufferInitialFillPercentage ::
                                    !(Maybe Nat),
                                    _msSlowPal :: !(Maybe Mpeg2SlowPal),
                                    _msParNumerator :: !(Maybe Nat),
                                    _msGopSize :: !(Maybe Double),
                                    _msNumberBFramesBetweenReferenceFrames ::
                                    !(Maybe Nat),
                                    _msGopSizeUnits ::
                                    !(Maybe Mpeg2GopSizeUnits),
                                    _msHrdBufferSize :: !(Maybe Nat),
                                    _msRateControlMode ::
                                    !(Maybe Mpeg2RateControlMode),
                                    _msTelecine :: !(Maybe Mpeg2Telecine),
                                    _msIntraDcPrecision ::
                                    !(Maybe Mpeg2IntraDcPrecision),
                                    _msDynamicSubGop ::
                                    !(Maybe Mpeg2DynamicSubGop),
                                    _msMinIInterval :: !(Maybe Nat),
                                    _msInterlaceMode ::
                                    !(Maybe Mpeg2InterlaceMode),
                                    _msParControl :: !(Maybe Mpeg2ParControl),
                                    _msSoftness :: !(Maybe Nat),
                                    _msCodecProfile ::
                                    !(Maybe Mpeg2CodecProfile),
                                    _msBitrate :: !(Maybe Nat),
                                    _msFramerateDenominator :: !(Maybe Nat),
                                    _msFramerateConversionAlgorithm ::
                                    !(Maybe Mpeg2FramerateConversionAlgorithm),
                                    _msCodecLevel :: !(Maybe Mpeg2CodecLevel),
                                    _msFramerateControl ::
                                    !(Maybe Mpeg2FramerateControl),
                                    _msAdaptiveQuantization ::
                                    !(Maybe Mpeg2AdaptiveQuantization),
                                    _msFramerateNumerator :: !(Maybe Nat),
                                    _msMaxBitrate :: !(Maybe Nat),
                                    _msSyntax :: !(Maybe Mpeg2Syntax),
                                    _msGopClosedCadence :: !(Maybe Nat),
                                    _msParDenominator :: !(Maybe Nat),
                                    _msSpatialAdaptiveQuantization ::
                                    !(Maybe Mpeg2SpatialAdaptiveQuantization)}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Mpeg2Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msQualityTuningLevel' - Use Quality tuning level (Mpeg2QualityTuningLevel) to specifiy whether to use single-pass or multipass video encoding.
--
-- * 'msTemporalAdaptiveQuantization' - Adjust quantization within each frame based on temporal variation of content complexity.
--
-- * 'msSceneChangeDetect' - Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default.
--
-- * 'msHrdBufferInitialFillPercentage' - Percentage of the buffer that should initially be filled (HRD buffer model).
--
-- * 'msSlowPal' - Enables Slow PAL rate conversion. 23.976fps and 24fps input is relabeled as 25fps, and audio is sped up correspondingly.
--
-- * 'msParNumerator' - Pixel Aspect Ratio numerator.
--
-- * 'msGopSize' - GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
--
-- * 'msNumberBFramesBetweenReferenceFrames' - Number of B-frames between reference frames.
--
-- * 'msGopSizeUnits' - Indicates if the GOP Size in MPEG2 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
--
-- * 'msHrdBufferSize' - Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
--
-- * 'msRateControlMode' - Use Rate control mode (Mpeg2RateControlMode) to specifiy whether the bitrate is variable (vbr) or constant (cbr).
--
-- * 'msTelecine' - Only use Telecine (Mpeg2Telecine) when you set Framerate (Framerate) to 29.970. Set Telecine (Mpeg2Telecine) to Hard (hard) to produce a 29.97i output from a 23.976 input. Set it to Soft (soft) to produce 23.976 output and leave converstion to the player.
--
-- * 'msIntraDcPrecision' - Use Intra DC precision (Mpeg2IntraDcPrecision) to set quantization precision for intra-block DC coefficients. If you choose the value auto, the service will automatically select the precision based on the per-frame compression ratio.
--
-- * 'msDynamicSubGop' - Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
--
-- * 'msMinIInterval' - Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
--
-- * 'msInterlaceMode' - Use Interlace mode (InterlaceMode) to choose the scan line type for the output. * Top Field First (TOP_FIELD) and Bottom Field First (BOTTOM_FIELD) produce interlaced output with the entire output having the same field polarity (top or bottom first). * Follow, Default Top (FOLLOW_TOP_FIELD) and Follow, Default Bottom (FOLLOW_BOTTOM_FIELD) use the same field polarity as the source. Therefore, behavior depends on the input scan type.   - If the source is interlaced, the output will be interlaced with the same polarity as the source (it will follow the source). The output could therefore be a mix of "top field first" and "bottom field first".   - If the source is progressive, the output will be interlaced with "top field first" or "bottom field first" polarity, depending on which of the Follow options you chose.
--
-- * 'msParControl' - Using the API, enable ParFollowSource if you want the service to use the pixel aspect ratio from the input. Using the console, do this by choosing Follow source for Pixel aspect ratio.
--
-- * 'msSoftness' - Softness. Selects quantizer matrix, larger values reduce high-frequency content in the encoded image.
--
-- * 'msCodecProfile' - Use Profile (Mpeg2CodecProfile) to set the MPEG-2 profile for the video output.
--
-- * 'msBitrate' - Specify the average bitrate in bits per second. Required for VBR and CBR. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
--
-- * 'msFramerateDenominator' - Frame rate denominator.
--
-- * 'msFramerateConversionAlgorithm' - When set to INTERPOLATE, produces smoother motion during frame rate conversion.
--
-- * 'msCodecLevel' - Use Level (Mpeg2CodecLevel) to set the MPEG-2 level for the video output.
--
-- * 'msFramerateControl' - If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job sepecification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
--
-- * 'msAdaptiveQuantization' - Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
--
-- * 'msFramerateNumerator' - Frame rate numerator - frame rate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
--
-- * 'msMaxBitrate' - Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000.
--
-- * 'msSyntax' - Produces a Type D-10 compatible bitstream (SMPTE 356M-2001).
--
-- * 'msGopClosedCadence' - Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
--
-- * 'msParDenominator' - Pixel Aspect Ratio denominator.
--
-- * 'msSpatialAdaptiveQuantization' - Adjust quantization within each frame based on spatial variation of content complexity.
mpeg2Settings
    :: Mpeg2Settings
mpeg2Settings
  = Mpeg2Settings'{_msQualityTuningLevel = Nothing,
                   _msTemporalAdaptiveQuantization = Nothing,
                   _msSceneChangeDetect = Nothing,
                   _msHrdBufferInitialFillPercentage = Nothing,
                   _msSlowPal = Nothing, _msParNumerator = Nothing,
                   _msGopSize = Nothing,
                   _msNumberBFramesBetweenReferenceFrames = Nothing,
                   _msGopSizeUnits = Nothing,
                   _msHrdBufferSize = Nothing,
                   _msRateControlMode = Nothing, _msTelecine = Nothing,
                   _msIntraDcPrecision = Nothing,
                   _msDynamicSubGop = Nothing,
                   _msMinIInterval = Nothing,
                   _msInterlaceMode = Nothing, _msParControl = Nothing,
                   _msSoftness = Nothing, _msCodecProfile = Nothing,
                   _msBitrate = Nothing,
                   _msFramerateDenominator = Nothing,
                   _msFramerateConversionAlgorithm = Nothing,
                   _msCodecLevel = Nothing,
                   _msFramerateControl = Nothing,
                   _msAdaptiveQuantization = Nothing,
                   _msFramerateNumerator = Nothing,
                   _msMaxBitrate = Nothing, _msSyntax = Nothing,
                   _msGopClosedCadence = Nothing,
                   _msParDenominator = Nothing,
                   _msSpatialAdaptiveQuantization = Nothing}

-- | Use Quality tuning level (Mpeg2QualityTuningLevel) to specifiy whether to use single-pass or multipass video encoding.
msQualityTuningLevel :: Lens' Mpeg2Settings (Maybe Mpeg2QualityTuningLevel)
msQualityTuningLevel = lens _msQualityTuningLevel (\ s a -> s{_msQualityTuningLevel = a})

-- | Adjust quantization within each frame based on temporal variation of content complexity.
msTemporalAdaptiveQuantization :: Lens' Mpeg2Settings (Maybe Mpeg2TemporalAdaptiveQuantization)
msTemporalAdaptiveQuantization = lens _msTemporalAdaptiveQuantization (\ s a -> s{_msTemporalAdaptiveQuantization = a})

-- | Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default.
msSceneChangeDetect :: Lens' Mpeg2Settings (Maybe Mpeg2SceneChangeDetect)
msSceneChangeDetect = lens _msSceneChangeDetect (\ s a -> s{_msSceneChangeDetect = a})

-- | Percentage of the buffer that should initially be filled (HRD buffer model).
msHrdBufferInitialFillPercentage :: Lens' Mpeg2Settings (Maybe Natural)
msHrdBufferInitialFillPercentage = lens _msHrdBufferInitialFillPercentage (\ s a -> s{_msHrdBufferInitialFillPercentage = a}) . mapping _Nat

-- | Enables Slow PAL rate conversion. 23.976fps and 24fps input is relabeled as 25fps, and audio is sped up correspondingly.
msSlowPal :: Lens' Mpeg2Settings (Maybe Mpeg2SlowPal)
msSlowPal = lens _msSlowPal (\ s a -> s{_msSlowPal = a})

-- | Pixel Aspect Ratio numerator.
msParNumerator :: Lens' Mpeg2Settings (Maybe Natural)
msParNumerator = lens _msParNumerator (\ s a -> s{_msParNumerator = a}) . mapping _Nat

-- | GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
msGopSize :: Lens' Mpeg2Settings (Maybe Double)
msGopSize = lens _msGopSize (\ s a -> s{_msGopSize = a})

-- | Number of B-frames between reference frames.
msNumberBFramesBetweenReferenceFrames :: Lens' Mpeg2Settings (Maybe Natural)
msNumberBFramesBetweenReferenceFrames = lens _msNumberBFramesBetweenReferenceFrames (\ s a -> s{_msNumberBFramesBetweenReferenceFrames = a}) . mapping _Nat

-- | Indicates if the GOP Size in MPEG2 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
msGopSizeUnits :: Lens' Mpeg2Settings (Maybe Mpeg2GopSizeUnits)
msGopSizeUnits = lens _msGopSizeUnits (\ s a -> s{_msGopSizeUnits = a})

-- | Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
msHrdBufferSize :: Lens' Mpeg2Settings (Maybe Natural)
msHrdBufferSize = lens _msHrdBufferSize (\ s a -> s{_msHrdBufferSize = a}) . mapping _Nat

-- | Use Rate control mode (Mpeg2RateControlMode) to specifiy whether the bitrate is variable (vbr) or constant (cbr).
msRateControlMode :: Lens' Mpeg2Settings (Maybe Mpeg2RateControlMode)
msRateControlMode = lens _msRateControlMode (\ s a -> s{_msRateControlMode = a})

-- | Only use Telecine (Mpeg2Telecine) when you set Framerate (Framerate) to 29.970. Set Telecine (Mpeg2Telecine) to Hard (hard) to produce a 29.97i output from a 23.976 input. Set it to Soft (soft) to produce 23.976 output and leave converstion to the player.
msTelecine :: Lens' Mpeg2Settings (Maybe Mpeg2Telecine)
msTelecine = lens _msTelecine (\ s a -> s{_msTelecine = a})

-- | Use Intra DC precision (Mpeg2IntraDcPrecision) to set quantization precision for intra-block DC coefficients. If you choose the value auto, the service will automatically select the precision based on the per-frame compression ratio.
msIntraDcPrecision :: Lens' Mpeg2Settings (Maybe Mpeg2IntraDcPrecision)
msIntraDcPrecision = lens _msIntraDcPrecision (\ s a -> s{_msIntraDcPrecision = a})

-- | Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
msDynamicSubGop :: Lens' Mpeg2Settings (Maybe Mpeg2DynamicSubGop)
msDynamicSubGop = lens _msDynamicSubGop (\ s a -> s{_msDynamicSubGop = a})

-- | Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
msMinIInterval :: Lens' Mpeg2Settings (Maybe Natural)
msMinIInterval = lens _msMinIInterval (\ s a -> s{_msMinIInterval = a}) . mapping _Nat

-- | Use Interlace mode (InterlaceMode) to choose the scan line type for the output. * Top Field First (TOP_FIELD) and Bottom Field First (BOTTOM_FIELD) produce interlaced output with the entire output having the same field polarity (top or bottom first). * Follow, Default Top (FOLLOW_TOP_FIELD) and Follow, Default Bottom (FOLLOW_BOTTOM_FIELD) use the same field polarity as the source. Therefore, behavior depends on the input scan type.   - If the source is interlaced, the output will be interlaced with the same polarity as the source (it will follow the source). The output could therefore be a mix of "top field first" and "bottom field first".   - If the source is progressive, the output will be interlaced with "top field first" or "bottom field first" polarity, depending on which of the Follow options you chose.
msInterlaceMode :: Lens' Mpeg2Settings (Maybe Mpeg2InterlaceMode)
msInterlaceMode = lens _msInterlaceMode (\ s a -> s{_msInterlaceMode = a})

-- | Using the API, enable ParFollowSource if you want the service to use the pixel aspect ratio from the input. Using the console, do this by choosing Follow source for Pixel aspect ratio.
msParControl :: Lens' Mpeg2Settings (Maybe Mpeg2ParControl)
msParControl = lens _msParControl (\ s a -> s{_msParControl = a})

-- | Softness. Selects quantizer matrix, larger values reduce high-frequency content in the encoded image.
msSoftness :: Lens' Mpeg2Settings (Maybe Natural)
msSoftness = lens _msSoftness (\ s a -> s{_msSoftness = a}) . mapping _Nat

-- | Use Profile (Mpeg2CodecProfile) to set the MPEG-2 profile for the video output.
msCodecProfile :: Lens' Mpeg2Settings (Maybe Mpeg2CodecProfile)
msCodecProfile = lens _msCodecProfile (\ s a -> s{_msCodecProfile = a})

-- | Specify the average bitrate in bits per second. Required for VBR and CBR. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
msBitrate :: Lens' Mpeg2Settings (Maybe Natural)
msBitrate = lens _msBitrate (\ s a -> s{_msBitrate = a}) . mapping _Nat

-- | Frame rate denominator.
msFramerateDenominator :: Lens' Mpeg2Settings (Maybe Natural)
msFramerateDenominator = lens _msFramerateDenominator (\ s a -> s{_msFramerateDenominator = a}) . mapping _Nat

-- | When set to INTERPOLATE, produces smoother motion during frame rate conversion.
msFramerateConversionAlgorithm :: Lens' Mpeg2Settings (Maybe Mpeg2FramerateConversionAlgorithm)
msFramerateConversionAlgorithm = lens _msFramerateConversionAlgorithm (\ s a -> s{_msFramerateConversionAlgorithm = a})

-- | Use Level (Mpeg2CodecLevel) to set the MPEG-2 level for the video output.
msCodecLevel :: Lens' Mpeg2Settings (Maybe Mpeg2CodecLevel)
msCodecLevel = lens _msCodecLevel (\ s a -> s{_msCodecLevel = a})

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job sepecification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
msFramerateControl :: Lens' Mpeg2Settings (Maybe Mpeg2FramerateControl)
msFramerateControl = lens _msFramerateControl (\ s a -> s{_msFramerateControl = a})

-- | Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
msAdaptiveQuantization :: Lens' Mpeg2Settings (Maybe Mpeg2AdaptiveQuantization)
msAdaptiveQuantization = lens _msAdaptiveQuantization (\ s a -> s{_msAdaptiveQuantization = a})

-- | Frame rate numerator - frame rate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
msFramerateNumerator :: Lens' Mpeg2Settings (Maybe Natural)
msFramerateNumerator = lens _msFramerateNumerator (\ s a -> s{_msFramerateNumerator = a}) . mapping _Nat

-- | Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000.
msMaxBitrate :: Lens' Mpeg2Settings (Maybe Natural)
msMaxBitrate = lens _msMaxBitrate (\ s a -> s{_msMaxBitrate = a}) . mapping _Nat

-- | Produces a Type D-10 compatible bitstream (SMPTE 356M-2001).
msSyntax :: Lens' Mpeg2Settings (Maybe Mpeg2Syntax)
msSyntax = lens _msSyntax (\ s a -> s{_msSyntax = a})

-- | Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
msGopClosedCadence :: Lens' Mpeg2Settings (Maybe Natural)
msGopClosedCadence = lens _msGopClosedCadence (\ s a -> s{_msGopClosedCadence = a}) . mapping _Nat

-- | Pixel Aspect Ratio denominator.
msParDenominator :: Lens' Mpeg2Settings (Maybe Natural)
msParDenominator = lens _msParDenominator (\ s a -> s{_msParDenominator = a}) . mapping _Nat

-- | Adjust quantization within each frame based on spatial variation of content complexity.
msSpatialAdaptiveQuantization :: Lens' Mpeg2Settings (Maybe Mpeg2SpatialAdaptiveQuantization)
msSpatialAdaptiveQuantization = lens _msSpatialAdaptiveQuantization (\ s a -> s{_msSpatialAdaptiveQuantization = a})

instance FromJSON Mpeg2Settings where
        parseJSON
          = withObject "Mpeg2Settings"
              (\ x ->
                 Mpeg2Settings' <$>
                   (x .:? "qualityTuningLevel") <*>
                     (x .:? "temporalAdaptiveQuantization")
                     <*> (x .:? "sceneChangeDetect")
                     <*> (x .:? "hrdBufferInitialFillPercentage")
                     <*> (x .:? "slowPal")
                     <*> (x .:? "parNumerator")
                     <*> (x .:? "gopSize")
                     <*> (x .:? "numberBFramesBetweenReferenceFrames")
                     <*> (x .:? "gopSizeUnits")
                     <*> (x .:? "hrdBufferSize")
                     <*> (x .:? "rateControlMode")
                     <*> (x .:? "telecine")
                     <*> (x .:? "intraDcPrecision")
                     <*> (x .:? "dynamicSubGop")
                     <*> (x .:? "minIInterval")
                     <*> (x .:? "interlaceMode")
                     <*> (x .:? "parControl")
                     <*> (x .:? "softness")
                     <*> (x .:? "codecProfile")
                     <*> (x .:? "bitrate")
                     <*> (x .:? "framerateDenominator")
                     <*> (x .:? "framerateConversionAlgorithm")
                     <*> (x .:? "codecLevel")
                     <*> (x .:? "framerateControl")
                     <*> (x .:? "adaptiveQuantization")
                     <*> (x .:? "framerateNumerator")
                     <*> (x .:? "maxBitrate")
                     <*> (x .:? "syntax")
                     <*> (x .:? "gopClosedCadence")
                     <*> (x .:? "parDenominator")
                     <*> (x .:? "spatialAdaptiveQuantization"))

instance Hashable Mpeg2Settings where

instance NFData Mpeg2Settings where

instance ToJSON Mpeg2Settings where
        toJSON Mpeg2Settings'{..}
          = object
              (catMaybes
                 [("qualityTuningLevel" .=) <$> _msQualityTuningLevel,
                  ("temporalAdaptiveQuantization" .=) <$>
                    _msTemporalAdaptiveQuantization,
                  ("sceneChangeDetect" .=) <$> _msSceneChangeDetect,
                  ("hrdBufferInitialFillPercentage" .=) <$>
                    _msHrdBufferInitialFillPercentage,
                  ("slowPal" .=) <$> _msSlowPal,
                  ("parNumerator" .=) <$> _msParNumerator,
                  ("gopSize" .=) <$> _msGopSize,
                  ("numberBFramesBetweenReferenceFrames" .=) <$>
                    _msNumberBFramesBetweenReferenceFrames,
                  ("gopSizeUnits" .=) <$> _msGopSizeUnits,
                  ("hrdBufferSize" .=) <$> _msHrdBufferSize,
                  ("rateControlMode" .=) <$> _msRateControlMode,
                  ("telecine" .=) <$> _msTelecine,
                  ("intraDcPrecision" .=) <$> _msIntraDcPrecision,
                  ("dynamicSubGop" .=) <$> _msDynamicSubGop,
                  ("minIInterval" .=) <$> _msMinIInterval,
                  ("interlaceMode" .=) <$> _msInterlaceMode,
                  ("parControl" .=) <$> _msParControl,
                  ("softness" .=) <$> _msSoftness,
                  ("codecProfile" .=) <$> _msCodecProfile,
                  ("bitrate" .=) <$> _msBitrate,
                  ("framerateDenominator" .=) <$>
                    _msFramerateDenominator,
                  ("framerateConversionAlgorithm" .=) <$>
                    _msFramerateConversionAlgorithm,
                  ("codecLevel" .=) <$> _msCodecLevel,
                  ("framerateControl" .=) <$> _msFramerateControl,
                  ("adaptiveQuantization" .=) <$>
                    _msAdaptiveQuantization,
                  ("framerateNumerator" .=) <$> _msFramerateNumerator,
                  ("maxBitrate" .=) <$> _msMaxBitrate,
                  ("syntax" .=) <$> _msSyntax,
                  ("gopClosedCadence" .=) <$> _msGopClosedCadence,
                  ("parDenominator" .=) <$> _msParDenominator,
                  ("spatialAdaptiveQuantization" .=) <$>
                    _msSpatialAdaptiveQuantization])
