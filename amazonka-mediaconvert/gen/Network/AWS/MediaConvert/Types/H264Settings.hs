{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264Settings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H264Settings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.H264AdaptiveQuantization
import Network.AWS.MediaConvert.Types.H264CodecLevel
import Network.AWS.MediaConvert.Types.H264CodecProfile
import Network.AWS.MediaConvert.Types.H264DynamicSubGop
import Network.AWS.MediaConvert.Types.H264EntropyEncoding
import Network.AWS.MediaConvert.Types.H264FieldEncoding
import Network.AWS.MediaConvert.Types.H264FlickerAdaptiveQuantization
import Network.AWS.MediaConvert.Types.H264FramerateControl
import Network.AWS.MediaConvert.Types.H264FramerateConversionAlgorithm
import Network.AWS.MediaConvert.Types.H264GopBReference
import Network.AWS.MediaConvert.Types.H264GopSizeUnits
import Network.AWS.MediaConvert.Types.H264InterlaceMode
import Network.AWS.MediaConvert.Types.H264ParControl
import Network.AWS.MediaConvert.Types.H264QualityTuningLevel
import Network.AWS.MediaConvert.Types.H264QvbrSettings
import Network.AWS.MediaConvert.Types.H264RateControlMode
import Network.AWS.MediaConvert.Types.H264RepeatPps
import Network.AWS.MediaConvert.Types.H264SceneChangeDetect
import Network.AWS.MediaConvert.Types.H264SlowPal
import Network.AWS.MediaConvert.Types.H264SpatialAdaptiveQuantization
import Network.AWS.MediaConvert.Types.H264Syntax
import Network.AWS.MediaConvert.Types.H264Telecine
import Network.AWS.MediaConvert.Types.H264TemporalAdaptiveQuantization
import Network.AWS.MediaConvert.Types.H264UnregisteredSeiTimecode
import Network.AWS.Prelude

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value H_264.
--
-- /See:/ 'h264Settings' smart constructor.
data H264Settings = H264Settings'{_hssUnregisteredSeiTimecode
                                  :: !(Maybe H264UnregisteredSeiTimecode),
                                  _hssQualityTuningLevel ::
                                  !(Maybe H264QualityTuningLevel),
                                  _hssTemporalAdaptiveQuantization ::
                                  !(Maybe H264TemporalAdaptiveQuantization),
                                  _hssSceneChangeDetect ::
                                  !(Maybe H264SceneChangeDetect),
                                  _hssHrdBufferInitialFillPercentage ::
                                  !(Maybe Nat),
                                  _hssSlowPal :: !(Maybe H264SlowPal),
                                  _hssParNumerator :: !(Maybe Nat),
                                  _hssGopSize :: !(Maybe Double),
                                  _hssNumberBFramesBetweenReferenceFrames ::
                                  !(Maybe Nat),
                                  _hssGopSizeUnits :: !(Maybe H264GopSizeUnits),
                                  _hssHrdBufferSize :: !(Maybe Nat),
                                  _hssSlices :: !(Maybe Nat),
                                  _hssRateControlMode ::
                                  !(Maybe H264RateControlMode),
                                  _hssNumberReferenceFrames :: !(Maybe Nat),
                                  _hssTelecine :: !(Maybe H264Telecine),
                                  _hssDynamicSubGop ::
                                  !(Maybe H264DynamicSubGop),
                                  _hssMinIInterval :: !(Maybe Nat),
                                  _hssInterlaceMode ::
                                  !(Maybe H264InterlaceMode),
                                  _hssParControl :: !(Maybe H264ParControl),
                                  _hssRepeatPps :: !(Maybe H264RepeatPps),
                                  _hssFlickerAdaptiveQuantization ::
                                  !(Maybe H264FlickerAdaptiveQuantization),
                                  _hssQvbrSettings :: !(Maybe H264QvbrSettings),
                                  _hssSoftness :: !(Maybe Nat),
                                  _hssCodecProfile :: !(Maybe H264CodecProfile),
                                  _hssBitrate :: !(Maybe Nat),
                                  _hssFramerateDenominator :: !(Maybe Nat),
                                  _hssFramerateConversionAlgorithm ::
                                  !(Maybe H264FramerateConversionAlgorithm),
                                  _hssCodecLevel :: !(Maybe H264CodecLevel),
                                  _hssEntropyEncoding ::
                                  !(Maybe H264EntropyEncoding),
                                  _hssFramerateControl ::
                                  !(Maybe H264FramerateControl),
                                  _hssAdaptiveQuantization ::
                                  !(Maybe H264AdaptiveQuantization),
                                  _hssFramerateNumerator :: !(Maybe Nat),
                                  _hssGopBReference ::
                                  !(Maybe H264GopBReference),
                                  _hssMaxBitrate :: !(Maybe Nat),
                                  _hssSyntax :: !(Maybe H264Syntax),
                                  _hssFieldEncoding ::
                                  !(Maybe H264FieldEncoding),
                                  _hssGopClosedCadence :: !(Maybe Nat),
                                  _hssParDenominator :: !(Maybe Nat),
                                  _hssSpatialAdaptiveQuantization ::
                                  !(Maybe H264SpatialAdaptiveQuantization)}
                      deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'H264Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hssUnregisteredSeiTimecode' - Inserts timecode for each frame as 4 bytes of an unregistered SEI message.
--
-- * 'hssQualityTuningLevel' - Use Quality tuning level (H264QualityTuningLevel) to specifiy whether to use fast single-pass, high-quality singlepass, or high-quality multipass video encoding.
--
-- * 'hssTemporalAdaptiveQuantization' - Adjust quantization within each frame based on temporal variation of content complexity.
--
-- * 'hssSceneChangeDetect' - Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default. If this output uses QVBR, choose Transition detection (TRANSITION_DETECTION) for further video quality improvement. For more information about QVBR, see https://docs.aws.amazon.com/console/mediaconvert/cbr-vbr-qvbr.
--
-- * 'hssHrdBufferInitialFillPercentage' - Percentage of the buffer that should initially be filled (HRD buffer model).
--
-- * 'hssSlowPal' - Enables Slow PAL rate conversion. 23.976fps and 24fps input is relabeled as 25fps, and audio is sped up correspondingly.
--
-- * 'hssParNumerator' - Pixel Aspect Ratio numerator.
--
-- * 'hssGopSize' - GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
--
-- * 'hssNumberBFramesBetweenReferenceFrames' - Number of B-frames between reference frames.
--
-- * 'hssGopSizeUnits' - Indicates if the GOP Size in H264 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
--
-- * 'hssHrdBufferSize' - Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
--
-- * 'hssSlices' - Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures.
--
-- * 'hssRateControlMode' - Use this setting to specify whether this output has a variable bitrate (VBR), constant bitrate (CBR) or quality-defined variable bitrate (QVBR).
--
-- * 'hssNumberReferenceFrames' - Number of reference frames to use. The encoder may use more than requested if using B-frames and/or interlaced encoding.
--
-- * 'hssTelecine' - This field applies only if the Streams > Advanced > Framerate (framerate) field  is set to 29.970. This field works with the Streams > Advanced > Preprocessors > Deinterlacer  field (deinterlace_mode) and the Streams > Advanced > Interlaced Mode field (interlace_mode)  to identify the scan type for the output: Progressive, Interlaced, Hard Telecine or Soft Telecine. - Hard: produces 29.97i output from 23.976 input. - Soft: produces 23.976; the player converts this output to 29.97i.
--
-- * 'hssDynamicSubGop' - Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
--
-- * 'hssMinIInterval' - Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
--
-- * 'hssInterlaceMode' - Use Interlace mode (InterlaceMode) to choose the scan line type for the output. * Top Field First (TOP_FIELD) and Bottom Field First (BOTTOM_FIELD) produce interlaced output with the entire output having the same field polarity (top or bottom first). * Follow, Default Top (FOLLOW_TOP_FIELD) and Follow, Default Bottom (FOLLOW_BOTTOM_FIELD) use the same field polarity as the source. Therefore, behavior depends on the input scan type, as follows.   - If the source is interlaced, the output will be interlaced with the same polarity as the source (it will follow the source). The output could therefore be a mix of "top field first" and "bottom field first".   - If the source is progressive, the output will be interlaced with "top field first" or "bottom field first" polarity, depending on which of the Follow options you chose.
--
-- * 'hssParControl' - Using the API, enable ParFollowSource if you want the service to use the pixel aspect ratio from the input. Using the console, do this by choosing Follow source for Pixel aspect ratio.
--
-- * 'hssRepeatPps' - Places a PPS header on each encoded picture, even if repeated.
--
-- * 'hssFlickerAdaptiveQuantization' - Adjust quantization within each frame to reduce flicker or 'pop' on I-frames.
--
-- * 'hssQvbrSettings' - Settings for quality-defined variable bitrate encoding with the H.264 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
--
-- * 'hssSoftness' - Softness. Selects quantizer matrix, larger values reduce high-frequency content in the encoded image.
--
-- * 'hssCodecProfile' - H.264 Profile. High 4:2:2 and 10-bit profiles are only available with the AVC-I License.
--
-- * 'hssBitrate' - Specify the average bitrate in bits per second. Required for VBR and CBR. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
--
-- * 'hssFramerateDenominator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- * 'hssFramerateConversionAlgorithm' - When set to INTERPOLATE, produces smoother motion during frame rate conversion.
--
-- * 'hssCodecLevel' - Specify an H.264 level that is consistent with your output video settings. If you aren't sure what level to specify, choose Auto (AUTO).
--
-- * 'hssEntropyEncoding' - Entropy encoding mode. Use CABAC (must be in Main or High profile) or CAVLC.
--
-- * 'hssFramerateControl' - If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
--
-- * 'hssAdaptiveQuantization' - Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
--
-- * 'hssFramerateNumerator' - Frame rate numerator - frame rate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
--
-- * 'hssGopBReference' - If enable, use reference B frames for GOP structures that have B frames > 1.
--
-- * 'hssMaxBitrate' - Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. Required when Rate control mode is QVBR.
--
-- * 'hssSyntax' - Produces a bitstream compliant with SMPTE RP-2027.
--
-- * 'hssFieldEncoding' - Choosing FORCE_FIELD disables PAFF encoding for interlaced outputs.
--
-- * 'hssGopClosedCadence' - Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
--
-- * 'hssParDenominator' - Pixel Aspect Ratio denominator.
--
-- * 'hssSpatialAdaptiveQuantization' - Adjust quantization within each frame based on spatial variation of content complexity.
h264Settings
    :: H264Settings
h264Settings
  = H264Settings'{_hssUnregisteredSeiTimecode =
                    Nothing,
                  _hssQualityTuningLevel = Nothing,
                  _hssTemporalAdaptiveQuantization = Nothing,
                  _hssSceneChangeDetect = Nothing,
                  _hssHrdBufferInitialFillPercentage = Nothing,
                  _hssSlowPal = Nothing, _hssParNumerator = Nothing,
                  _hssGopSize = Nothing,
                  _hssNumberBFramesBetweenReferenceFrames = Nothing,
                  _hssGopSizeUnits = Nothing,
                  _hssHrdBufferSize = Nothing, _hssSlices = Nothing,
                  _hssRateControlMode = Nothing,
                  _hssNumberReferenceFrames = Nothing,
                  _hssTelecine = Nothing, _hssDynamicSubGop = Nothing,
                  _hssMinIInterval = Nothing,
                  _hssInterlaceMode = Nothing,
                  _hssParControl = Nothing, _hssRepeatPps = Nothing,
                  _hssFlickerAdaptiveQuantization = Nothing,
                  _hssQvbrSettings = Nothing, _hssSoftness = Nothing,
                  _hssCodecProfile = Nothing, _hssBitrate = Nothing,
                  _hssFramerateDenominator = Nothing,
                  _hssFramerateConversionAlgorithm = Nothing,
                  _hssCodecLevel = Nothing,
                  _hssEntropyEncoding = Nothing,
                  _hssFramerateControl = Nothing,
                  _hssAdaptiveQuantization = Nothing,
                  _hssFramerateNumerator = Nothing,
                  _hssGopBReference = Nothing,
                  _hssMaxBitrate = Nothing, _hssSyntax = Nothing,
                  _hssFieldEncoding = Nothing,
                  _hssGopClosedCadence = Nothing,
                  _hssParDenominator = Nothing,
                  _hssSpatialAdaptiveQuantization = Nothing}

-- | Inserts timecode for each frame as 4 bytes of an unregistered SEI message.
hssUnregisteredSeiTimecode :: Lens' H264Settings (Maybe H264UnregisteredSeiTimecode)
hssUnregisteredSeiTimecode = lens _hssUnregisteredSeiTimecode (\ s a -> s{_hssUnregisteredSeiTimecode = a})

-- | Use Quality tuning level (H264QualityTuningLevel) to specifiy whether to use fast single-pass, high-quality singlepass, or high-quality multipass video encoding.
hssQualityTuningLevel :: Lens' H264Settings (Maybe H264QualityTuningLevel)
hssQualityTuningLevel = lens _hssQualityTuningLevel (\ s a -> s{_hssQualityTuningLevel = a})

-- | Adjust quantization within each frame based on temporal variation of content complexity.
hssTemporalAdaptiveQuantization :: Lens' H264Settings (Maybe H264TemporalAdaptiveQuantization)
hssTemporalAdaptiveQuantization = lens _hssTemporalAdaptiveQuantization (\ s a -> s{_hssTemporalAdaptiveQuantization = a})

-- | Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default. If this output uses QVBR, choose Transition detection (TRANSITION_DETECTION) for further video quality improvement. For more information about QVBR, see https://docs.aws.amazon.com/console/mediaconvert/cbr-vbr-qvbr.
hssSceneChangeDetect :: Lens' H264Settings (Maybe H264SceneChangeDetect)
hssSceneChangeDetect = lens _hssSceneChangeDetect (\ s a -> s{_hssSceneChangeDetect = a})

-- | Percentage of the buffer that should initially be filled (HRD buffer model).
hssHrdBufferInitialFillPercentage :: Lens' H264Settings (Maybe Natural)
hssHrdBufferInitialFillPercentage = lens _hssHrdBufferInitialFillPercentage (\ s a -> s{_hssHrdBufferInitialFillPercentage = a}) . mapping _Nat

-- | Enables Slow PAL rate conversion. 23.976fps and 24fps input is relabeled as 25fps, and audio is sped up correspondingly.
hssSlowPal :: Lens' H264Settings (Maybe H264SlowPal)
hssSlowPal = lens _hssSlowPal (\ s a -> s{_hssSlowPal = a})

-- | Pixel Aspect Ratio numerator.
hssParNumerator :: Lens' H264Settings (Maybe Natural)
hssParNumerator = lens _hssParNumerator (\ s a -> s{_hssParNumerator = a}) . mapping _Nat

-- | GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
hssGopSize :: Lens' H264Settings (Maybe Double)
hssGopSize = lens _hssGopSize (\ s a -> s{_hssGopSize = a})

-- | Number of B-frames between reference frames.
hssNumberBFramesBetweenReferenceFrames :: Lens' H264Settings (Maybe Natural)
hssNumberBFramesBetweenReferenceFrames = lens _hssNumberBFramesBetweenReferenceFrames (\ s a -> s{_hssNumberBFramesBetweenReferenceFrames = a}) . mapping _Nat

-- | Indicates if the GOP Size in H264 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
hssGopSizeUnits :: Lens' H264Settings (Maybe H264GopSizeUnits)
hssGopSizeUnits = lens _hssGopSizeUnits (\ s a -> s{_hssGopSizeUnits = a})

-- | Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
hssHrdBufferSize :: Lens' H264Settings (Maybe Natural)
hssHrdBufferSize = lens _hssHrdBufferSize (\ s a -> s{_hssHrdBufferSize = a}) . mapping _Nat

-- | Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures.
hssSlices :: Lens' H264Settings (Maybe Natural)
hssSlices = lens _hssSlices (\ s a -> s{_hssSlices = a}) . mapping _Nat

-- | Use this setting to specify whether this output has a variable bitrate (VBR), constant bitrate (CBR) or quality-defined variable bitrate (QVBR).
hssRateControlMode :: Lens' H264Settings (Maybe H264RateControlMode)
hssRateControlMode = lens _hssRateControlMode (\ s a -> s{_hssRateControlMode = a})

-- | Number of reference frames to use. The encoder may use more than requested if using B-frames and/or interlaced encoding.
hssNumberReferenceFrames :: Lens' H264Settings (Maybe Natural)
hssNumberReferenceFrames = lens _hssNumberReferenceFrames (\ s a -> s{_hssNumberReferenceFrames = a}) . mapping _Nat

-- | This field applies only if the Streams > Advanced > Framerate (framerate) field  is set to 29.970. This field works with the Streams > Advanced > Preprocessors > Deinterlacer  field (deinterlace_mode) and the Streams > Advanced > Interlaced Mode field (interlace_mode)  to identify the scan type for the output: Progressive, Interlaced, Hard Telecine or Soft Telecine. - Hard: produces 29.97i output from 23.976 input. - Soft: produces 23.976; the player converts this output to 29.97i.
hssTelecine :: Lens' H264Settings (Maybe H264Telecine)
hssTelecine = lens _hssTelecine (\ s a -> s{_hssTelecine = a})

-- | Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
hssDynamicSubGop :: Lens' H264Settings (Maybe H264DynamicSubGop)
hssDynamicSubGop = lens _hssDynamicSubGop (\ s a -> s{_hssDynamicSubGop = a})

-- | Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
hssMinIInterval :: Lens' H264Settings (Maybe Natural)
hssMinIInterval = lens _hssMinIInterval (\ s a -> s{_hssMinIInterval = a}) . mapping _Nat

-- | Use Interlace mode (InterlaceMode) to choose the scan line type for the output. * Top Field First (TOP_FIELD) and Bottom Field First (BOTTOM_FIELD) produce interlaced output with the entire output having the same field polarity (top or bottom first). * Follow, Default Top (FOLLOW_TOP_FIELD) and Follow, Default Bottom (FOLLOW_BOTTOM_FIELD) use the same field polarity as the source. Therefore, behavior depends on the input scan type, as follows.   - If the source is interlaced, the output will be interlaced with the same polarity as the source (it will follow the source). The output could therefore be a mix of "top field first" and "bottom field first".   - If the source is progressive, the output will be interlaced with "top field first" or "bottom field first" polarity, depending on which of the Follow options you chose.
hssInterlaceMode :: Lens' H264Settings (Maybe H264InterlaceMode)
hssInterlaceMode = lens _hssInterlaceMode (\ s a -> s{_hssInterlaceMode = a})

-- | Using the API, enable ParFollowSource if you want the service to use the pixel aspect ratio from the input. Using the console, do this by choosing Follow source for Pixel aspect ratio.
hssParControl :: Lens' H264Settings (Maybe H264ParControl)
hssParControl = lens _hssParControl (\ s a -> s{_hssParControl = a})

-- | Places a PPS header on each encoded picture, even if repeated.
hssRepeatPps :: Lens' H264Settings (Maybe H264RepeatPps)
hssRepeatPps = lens _hssRepeatPps (\ s a -> s{_hssRepeatPps = a})

-- | Adjust quantization within each frame to reduce flicker or 'pop' on I-frames.
hssFlickerAdaptiveQuantization :: Lens' H264Settings (Maybe H264FlickerAdaptiveQuantization)
hssFlickerAdaptiveQuantization = lens _hssFlickerAdaptiveQuantization (\ s a -> s{_hssFlickerAdaptiveQuantization = a})

-- | Settings for quality-defined variable bitrate encoding with the H.264 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
hssQvbrSettings :: Lens' H264Settings (Maybe H264QvbrSettings)
hssQvbrSettings = lens _hssQvbrSettings (\ s a -> s{_hssQvbrSettings = a})

-- | Softness. Selects quantizer matrix, larger values reduce high-frequency content in the encoded image.
hssSoftness :: Lens' H264Settings (Maybe Natural)
hssSoftness = lens _hssSoftness (\ s a -> s{_hssSoftness = a}) . mapping _Nat

-- | H.264 Profile. High 4:2:2 and 10-bit profiles are only available with the AVC-I License.
hssCodecProfile :: Lens' H264Settings (Maybe H264CodecProfile)
hssCodecProfile = lens _hssCodecProfile (\ s a -> s{_hssCodecProfile = a})

-- | Specify the average bitrate in bits per second. Required for VBR and CBR. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
hssBitrate :: Lens' H264Settings (Maybe Natural)
hssBitrate = lens _hssBitrate (\ s a -> s{_hssBitrate = a}) . mapping _Nat

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
hssFramerateDenominator :: Lens' H264Settings (Maybe Natural)
hssFramerateDenominator = lens _hssFramerateDenominator (\ s a -> s{_hssFramerateDenominator = a}) . mapping _Nat

-- | When set to INTERPOLATE, produces smoother motion during frame rate conversion.
hssFramerateConversionAlgorithm :: Lens' H264Settings (Maybe H264FramerateConversionAlgorithm)
hssFramerateConversionAlgorithm = lens _hssFramerateConversionAlgorithm (\ s a -> s{_hssFramerateConversionAlgorithm = a})

-- | Specify an H.264 level that is consistent with your output video settings. If you aren't sure what level to specify, choose Auto (AUTO).
hssCodecLevel :: Lens' H264Settings (Maybe H264CodecLevel)
hssCodecLevel = lens _hssCodecLevel (\ s a -> s{_hssCodecLevel = a})

-- | Entropy encoding mode. Use CABAC (must be in Main or High profile) or CAVLC.
hssEntropyEncoding :: Lens' H264Settings (Maybe H264EntropyEncoding)
hssEntropyEncoding = lens _hssEntropyEncoding (\ s a -> s{_hssEntropyEncoding = a})

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
hssFramerateControl :: Lens' H264Settings (Maybe H264FramerateControl)
hssFramerateControl = lens _hssFramerateControl (\ s a -> s{_hssFramerateControl = a})

-- | Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
hssAdaptiveQuantization :: Lens' H264Settings (Maybe H264AdaptiveQuantization)
hssAdaptiveQuantization = lens _hssAdaptiveQuantization (\ s a -> s{_hssAdaptiveQuantization = a})

-- | Frame rate numerator - frame rate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
hssFramerateNumerator :: Lens' H264Settings (Maybe Natural)
hssFramerateNumerator = lens _hssFramerateNumerator (\ s a -> s{_hssFramerateNumerator = a}) . mapping _Nat

-- | If enable, use reference B frames for GOP structures that have B frames > 1.
hssGopBReference :: Lens' H264Settings (Maybe H264GopBReference)
hssGopBReference = lens _hssGopBReference (\ s a -> s{_hssGopBReference = a})

-- | Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. Required when Rate control mode is QVBR.
hssMaxBitrate :: Lens' H264Settings (Maybe Natural)
hssMaxBitrate = lens _hssMaxBitrate (\ s a -> s{_hssMaxBitrate = a}) . mapping _Nat

-- | Produces a bitstream compliant with SMPTE RP-2027.
hssSyntax :: Lens' H264Settings (Maybe H264Syntax)
hssSyntax = lens _hssSyntax (\ s a -> s{_hssSyntax = a})

-- | Choosing FORCE_FIELD disables PAFF encoding for interlaced outputs.
hssFieldEncoding :: Lens' H264Settings (Maybe H264FieldEncoding)
hssFieldEncoding = lens _hssFieldEncoding (\ s a -> s{_hssFieldEncoding = a})

-- | Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
hssGopClosedCadence :: Lens' H264Settings (Maybe Natural)
hssGopClosedCadence = lens _hssGopClosedCadence (\ s a -> s{_hssGopClosedCadence = a}) . mapping _Nat

-- | Pixel Aspect Ratio denominator.
hssParDenominator :: Lens' H264Settings (Maybe Natural)
hssParDenominator = lens _hssParDenominator (\ s a -> s{_hssParDenominator = a}) . mapping _Nat

-- | Adjust quantization within each frame based on spatial variation of content complexity.
hssSpatialAdaptiveQuantization :: Lens' H264Settings (Maybe H264SpatialAdaptiveQuantization)
hssSpatialAdaptiveQuantization = lens _hssSpatialAdaptiveQuantization (\ s a -> s{_hssSpatialAdaptiveQuantization = a})

instance FromJSON H264Settings where
        parseJSON
          = withObject "H264Settings"
              (\ x ->
                 H264Settings' <$>
                   (x .:? "unregisteredSeiTimecode") <*>
                     (x .:? "qualityTuningLevel")
                     <*> (x .:? "temporalAdaptiveQuantization")
                     <*> (x .:? "sceneChangeDetect")
                     <*> (x .:? "hrdBufferInitialFillPercentage")
                     <*> (x .:? "slowPal")
                     <*> (x .:? "parNumerator")
                     <*> (x .:? "gopSize")
                     <*> (x .:? "numberBFramesBetweenReferenceFrames")
                     <*> (x .:? "gopSizeUnits")
                     <*> (x .:? "hrdBufferSize")
                     <*> (x .:? "slices")
                     <*> (x .:? "rateControlMode")
                     <*> (x .:? "numberReferenceFrames")
                     <*> (x .:? "telecine")
                     <*> (x .:? "dynamicSubGop")
                     <*> (x .:? "minIInterval")
                     <*> (x .:? "interlaceMode")
                     <*> (x .:? "parControl")
                     <*> (x .:? "repeatPps")
                     <*> (x .:? "flickerAdaptiveQuantization")
                     <*> (x .:? "qvbrSettings")
                     <*> (x .:? "softness")
                     <*> (x .:? "codecProfile")
                     <*> (x .:? "bitrate")
                     <*> (x .:? "framerateDenominator")
                     <*> (x .:? "framerateConversionAlgorithm")
                     <*> (x .:? "codecLevel")
                     <*> (x .:? "entropyEncoding")
                     <*> (x .:? "framerateControl")
                     <*> (x .:? "adaptiveQuantization")
                     <*> (x .:? "framerateNumerator")
                     <*> (x .:? "gopBReference")
                     <*> (x .:? "maxBitrate")
                     <*> (x .:? "syntax")
                     <*> (x .:? "fieldEncoding")
                     <*> (x .:? "gopClosedCadence")
                     <*> (x .:? "parDenominator")
                     <*> (x .:? "spatialAdaptiveQuantization"))

instance Hashable H264Settings where

instance NFData H264Settings where

instance ToJSON H264Settings where
        toJSON H264Settings'{..}
          = object
              (catMaybes
                 [("unregisteredSeiTimecode" .=) <$>
                    _hssUnregisteredSeiTimecode,
                  ("qualityTuningLevel" .=) <$> _hssQualityTuningLevel,
                  ("temporalAdaptiveQuantization" .=) <$>
                    _hssTemporalAdaptiveQuantization,
                  ("sceneChangeDetect" .=) <$> _hssSceneChangeDetect,
                  ("hrdBufferInitialFillPercentage" .=) <$>
                    _hssHrdBufferInitialFillPercentage,
                  ("slowPal" .=) <$> _hssSlowPal,
                  ("parNumerator" .=) <$> _hssParNumerator,
                  ("gopSize" .=) <$> _hssGopSize,
                  ("numberBFramesBetweenReferenceFrames" .=) <$>
                    _hssNumberBFramesBetweenReferenceFrames,
                  ("gopSizeUnits" .=) <$> _hssGopSizeUnits,
                  ("hrdBufferSize" .=) <$> _hssHrdBufferSize,
                  ("slices" .=) <$> _hssSlices,
                  ("rateControlMode" .=) <$> _hssRateControlMode,
                  ("numberReferenceFrames" .=) <$>
                    _hssNumberReferenceFrames,
                  ("telecine" .=) <$> _hssTelecine,
                  ("dynamicSubGop" .=) <$> _hssDynamicSubGop,
                  ("minIInterval" .=) <$> _hssMinIInterval,
                  ("interlaceMode" .=) <$> _hssInterlaceMode,
                  ("parControl" .=) <$> _hssParControl,
                  ("repeatPps" .=) <$> _hssRepeatPps,
                  ("flickerAdaptiveQuantization" .=) <$>
                    _hssFlickerAdaptiveQuantization,
                  ("qvbrSettings" .=) <$> _hssQvbrSettings,
                  ("softness" .=) <$> _hssSoftness,
                  ("codecProfile" .=) <$> _hssCodecProfile,
                  ("bitrate" .=) <$> _hssBitrate,
                  ("framerateDenominator" .=) <$>
                    _hssFramerateDenominator,
                  ("framerateConversionAlgorithm" .=) <$>
                    _hssFramerateConversionAlgorithm,
                  ("codecLevel" .=) <$> _hssCodecLevel,
                  ("entropyEncoding" .=) <$> _hssEntropyEncoding,
                  ("framerateControl" .=) <$> _hssFramerateControl,
                  ("adaptiveQuantization" .=) <$>
                    _hssAdaptiveQuantization,
                  ("framerateNumerator" .=) <$> _hssFramerateNumerator,
                  ("gopBReference" .=) <$> _hssGopBReference,
                  ("maxBitrate" .=) <$> _hssMaxBitrate,
                  ("syntax" .=) <$> _hssSyntax,
                  ("fieldEncoding" .=) <$> _hssFieldEncoding,
                  ("gopClosedCadence" .=) <$> _hssGopClosedCadence,
                  ("parDenominator" .=) <$> _hssParDenominator,
                  ("spatialAdaptiveQuantization" .=) <$>
                    _hssSpatialAdaptiveQuantization])
