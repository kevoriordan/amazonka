{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsSettings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.M2tsSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.DvbNitSettings
import Network.AWS.MediaLive.Types.DvbSdtSettings
import Network.AWS.MediaLive.Types.DvbTdtSettings
import Network.AWS.MediaLive.Types.M2tsAbsentInputAudioBehavior
import Network.AWS.MediaLive.Types.M2tsArib
import Network.AWS.MediaLive.Types.M2tsAribCaptionsPidControl
import Network.AWS.MediaLive.Types.M2tsAudioBufferModel
import Network.AWS.MediaLive.Types.M2tsAudioInterval
import Network.AWS.MediaLive.Types.M2tsAudioStreamType
import Network.AWS.MediaLive.Types.M2tsBufferModel
import Network.AWS.MediaLive.Types.M2tsCCDescriptor
import Network.AWS.MediaLive.Types.M2tsEbifControl
import Network.AWS.MediaLive.Types.M2tsEbpPlacement
import Network.AWS.MediaLive.Types.M2tsEsRateInPes
import Network.AWS.MediaLive.Types.M2tsKlv
import Network.AWS.MediaLive.Types.M2tsNielsenId3Behavior
import Network.AWS.MediaLive.Types.M2tsPcrControl
import Network.AWS.MediaLive.Types.M2tsRateMode
import Network.AWS.MediaLive.Types.M2tsScte35Control
import Network.AWS.MediaLive.Types.M2tsSegmentationMarkers
import Network.AWS.MediaLive.Types.M2tsSegmentationStyle
import Network.AWS.MediaLive.Types.M2tsTimedMetadataBehavior
import Network.AWS.Prelude

-- | M2ts Settings
--
-- /See:/ 'm2tsSettings' smart constructor.
data M2tsSettings = M2tsSettings'{_msPmtPid ::
                                  !(Maybe Text),
                                  _msEtvSignalPid :: !(Maybe Text),
                                  _msVideoPid :: !(Maybe Text),
                                  _msNielsenId3Behavior ::
                                  !(Maybe M2tsNielsenId3Behavior),
                                  _msBufferModel :: !(Maybe M2tsBufferModel),
                                  _msScte35Pid :: !(Maybe Text),
                                  _msTransportStreamId :: !(Maybe Nat),
                                  _msProgramNum :: !(Maybe Nat),
                                  _msFragmentTime :: !(Maybe Double),
                                  _msTimedMetadataBehavior ::
                                  !(Maybe M2tsTimedMetadataBehavior),
                                  _msCCDescriptor :: !(Maybe M2tsCCDescriptor),
                                  _msPmtInterval :: !(Maybe Nat),
                                  _msDvbSdtSettings :: !(Maybe DvbSdtSettings),
                                  _msEcmPid :: !(Maybe Text),
                                  _msNullPacketBitrate :: !(Maybe Double),
                                  _msAudioBufferModel ::
                                  !(Maybe M2tsAudioBufferModel),
                                  _msTimedMetadataPid :: !(Maybe Text),
                                  _msKlv :: !(Maybe M2tsKlv),
                                  _msAudioFramesPerPes :: !(Maybe Nat),
                                  _msPcrPeriod :: !(Maybe Nat),
                                  _msPcrPid :: !(Maybe Text),
                                  _msSegmentationMarkers ::
                                  !(Maybe M2tsSegmentationMarkers),
                                  _msAribCaptionsPidControl ::
                                  !(Maybe M2tsAribCaptionsPidControl),
                                  _msKlvDataPids :: !(Maybe Text),
                                  _msEbpLookaheadMs :: !(Maybe Nat),
                                  _msDvbSubPids :: !(Maybe Text),
                                  _msScte27Pids :: !(Maybe Text),
                                  _msPatInterval :: !(Maybe Nat),
                                  _msAudioStreamType ::
                                  !(Maybe M2tsAudioStreamType),
                                  _msEsRateInPes :: !(Maybe M2tsEsRateInPes),
                                  _msEtvPlatformPid :: !(Maybe Text),
                                  _msBitrate :: !(Maybe Nat),
                                  _msScte35Control ::
                                  !(Maybe M2tsScte35Control),
                                  _msAudioPids :: !(Maybe Text),
                                  _msDvbTeletextPid :: !(Maybe Text),
                                  _msEbif :: !(Maybe M2tsEbifControl),
                                  _msArib :: !(Maybe M2tsArib),
                                  _msAribCaptionsPid :: !(Maybe Text),
                                  _msAbsentInputAudioBehavior ::
                                  !(Maybe M2tsAbsentInputAudioBehavior),
                                  _msSegmentationTime :: !(Maybe Double),
                                  _msEbpAudioInterval ::
                                  !(Maybe M2tsAudioInterval),
                                  _msDvbNitSettings :: !(Maybe DvbNitSettings),
                                  _msPcrControl :: !(Maybe M2tsPcrControl),
                                  _msEbpPlacement :: !(Maybe M2tsEbpPlacement),
                                  _msRateMode :: !(Maybe M2tsRateMode),
                                  _msSegmentationStyle ::
                                  !(Maybe M2tsSegmentationStyle),
                                  _msDvbTdtSettings :: !(Maybe DvbTdtSettings)}
                      deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'M2tsSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msPmtPid' - Packet Identifier (PID) for the Program Map Table (PMT) in the transport stream. Can be entered as a decimal or hexadecimal value. Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- * 'msEtvSignalPid' - Packet Identifier (PID) for input source ETV Signal data to this output. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- * 'msVideoPid' - Packet Identifier (PID) of the elementary video stream in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- * 'msNielsenId3Behavior' - If set to passthrough, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
--
-- * 'msBufferModel' - If set to multiplex, use multiplex buffer model for accurate interleaving.  Setting to bufferModel to none can lead to lower latency, but low-memory devices may not be able to play back the stream without interruptions.
--
-- * 'msScte35Pid' - Packet Identifier (PID) of the SCTE-35 stream in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- * 'msTransportStreamId' - The value of the transport stream ID field in the Program Map Table.
--
-- * 'msProgramNum' - The value of the program number field in the Program Map Table.
--
-- * 'msFragmentTime' - The length in seconds of each fragment. Only used with EBP markers.
--
-- * 'msTimedMetadataBehavior' - When set to passthrough, timed metadata will be passed through from input to output.
--
-- * 'msCCDescriptor' - When set to enabled, generates captionServiceDescriptor in PMT.
--
-- * 'msPmtInterval' - The number of milliseconds between instances of this table in the output transport stream. Valid values are 0, 10..1000.
--
-- * 'msDvbSdtSettings' - Inserts DVB Service Description Table (SDT) at the specified table repetition interval.
--
-- * 'msEcmPid' - This field is unused and deprecated.
--
-- * 'msNullPacketBitrate' - Value in bits per second of extra null packets to insert into the transport stream. This can be used if a downstream encryption system requires periodic null packets.
--
-- * 'msAudioBufferModel' - When set to dvb, uses DVB buffer model for Dolby Digital audio.  When set to atsc, the ATSC model is used.
--
-- * 'msTimedMetadataPid' - Packet Identifier (PID) of the timed metadata stream in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- * 'msKlv' - If set to passthrough, passes any KLV data from the input source to this output.
--
-- * 'msAudioFramesPerPes' - The number of audio frames to insert for each PES packet.
--
-- * 'msPcrPeriod' - Maximum time in milliseconds between Program Clock Reference (PCRs) inserted into the transport stream.
--
-- * 'msPcrPid' - Packet Identifier (PID) of the Program Clock Reference (PCR) in the transport stream. When no value is given, the encoder will assign the same value as the Video PID. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- * 'msSegmentationMarkers' - Inserts segmentation markers at each segmentationTime period. raiSegstart sets the Random Access Indicator bit in the adaptation field. raiAdapt sets the RAI bit and adds the current timecode in the private data bytes. psiSegstart inserts PAT and PMT tables at the start of segments. ebp adds Encoder Boundary Point information to the adaptation field as per OpenCable specification OC-SP-EBP-I01-130118. ebpLegacy adds Encoder Boundary Point information to the adaptation field using a legacy proprietary format.
--
-- * 'msAribCaptionsPidControl' - If set to auto, pid number used for ARIB Captions will be auto-selected from unused pids.  If set to useConfigured, ARIB Captions will be on the configured pid number.
--
-- * 'msKlvDataPids' - Packet Identifier (PID) for input source KLV data to this output. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values.  Each PID specified must be in the range of 32 (or 0x20)..8182 (or 0x1ff6).
--
-- * 'msEbpLookaheadMs' - When set, enforces that Encoder Boundary Points do not come within the specified time interval of each other by looking ahead at input video. If another EBP is going to come in within the specified time interval, the current EBP is not emitted, and the segment is "stretched" to the next marker.  The lookahead value does not add latency to the system. The Live Event must be configured elsewhere to create sufficient latency to make the lookahead accurate.
--
-- * 'msDvbSubPids' - Packet Identifier (PID) for input source DVB Subtitle data to this output. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values.  Each PID specified must be in the range of 32 (or 0x20)..8182 (or 0x1ff6).
--
-- * 'msScte27Pids' - Packet Identifier (PID) for input source SCTE-27 data to this output. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values.  Each PID specified must be in the range of 32 (or 0x20)..8182 (or 0x1ff6).
--
-- * 'msPatInterval' - The number of milliseconds between instances of this table in the output transport stream.  Valid values are 0, 10..1000.
--
-- * 'msAudioStreamType' - When set to atsc, uses stream type = 0x81 for AC3 and stream type = 0x87 for EAC3. When set to dvb, uses stream type = 0x06.
--
-- * 'msEsRateInPes' - Include or exclude the ES Rate field in the PES header.
--
-- * 'msEtvPlatformPid' - Packet Identifier (PID) for input source ETV Platform data to this output. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- * 'msBitrate' - The output bitrate of the transport stream in bits per second. Setting to 0 lets the muxer automatically determine the appropriate bitrate.
--
-- * 'msScte35Control' - Optionally pass SCTE-35 signals from the input source to this output.
--
-- * 'msAudioPids' - Packet Identifier (PID) of the elementary audio stream(s) in the transport stream. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values. Each PID specified must be in the range of 32 (or 0x20)..8182 (or 0x1ff6).
--
-- * 'msDvbTeletextPid' - Packet Identifier (PID) for input source DVB Teletext data to this output. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- * 'msEbif' - If set to passthrough, passes any EBIF data from the input source to this output.
--
-- * 'msArib' - When set to enabled, uses ARIB-compliant field muxing and removes video descriptor.
--
-- * 'msAribCaptionsPid' - Packet Identifier (PID) for ARIB Captions in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- * 'msAbsentInputAudioBehavior' - When set to drop, output audio streams will be removed from the program if the selected input audio stream is removed from the input. This allows the output audio configuration to dynamically change based on input configuration. If this is set to encodeSilence, all output audio streams will output encoded silence when not connected to an active input stream.
--
-- * 'msSegmentationTime' - The length in seconds of each segment. Required unless markers is set to None_.
--
-- * 'msEbpAudioInterval' - When videoAndFixedIntervals is selected, audio EBP markers will be added to partitions 3 and 4. The interval between these additional markers will be fixed, and will be slightly shorter than the video EBP marker interval. Only available when EBP Cablelabs segmentation markers are selected.  Partitions 1 and 2 will always follow the video interval.
--
-- * 'msDvbNitSettings' - Inserts DVB Network Information Table (NIT) at the specified table repetition interval.
--
-- * 'msPcrControl' - When set to pcrEveryPesPacket, a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.
--
-- * 'msEbpPlacement' - Controls placement of EBP on Audio PIDs. If set to videoAndAudioPids, EBP markers will be placed on the video PID and all audio PIDs.  If set to videoPid, EBP markers will be placed on only the video PID.
--
-- * 'msRateMode' - When vbr, does not insert null packets into transport stream to fill specified bitrate. The bitrate setting acts as the maximum bitrate when vbr is set.
--
-- * 'msSegmentationStyle' - The segmentation style parameter controls how segmentation markers are inserted into the transport stream. With avails, it is possible that segments may be truncated, which can influence where future segmentation markers are inserted. When a segmentation style of "resetCadence" is selected and a segment is truncated due to an avail, we will reset the segmentation cadence. This means the subsequent segment will have a duration of $segmentationTime seconds. When a segmentation style of "maintainCadence" is selected and a segment is truncated due to an avail, we will not reset the segmentation cadence. This means the subsequent segment will likely be truncated as well. However, all segments after that will have a duration of $segmentationTime seconds. Note that EBP lookahead is a slight exception to this rule.
--
-- * 'msDvbTdtSettings' - Inserts DVB Time and Date Table (TDT) at the specified table repetition interval.
m2tsSettings
    :: M2tsSettings
m2tsSettings
  = M2tsSettings'{_msPmtPid = Nothing,
                  _msEtvSignalPid = Nothing, _msVideoPid = Nothing,
                  _msNielsenId3Behavior = Nothing,
                  _msBufferModel = Nothing, _msScte35Pid = Nothing,
                  _msTransportStreamId = Nothing,
                  _msProgramNum = Nothing, _msFragmentTime = Nothing,
                  _msTimedMetadataBehavior = Nothing,
                  _msCCDescriptor = Nothing, _msPmtInterval = Nothing,
                  _msDvbSdtSettings = Nothing, _msEcmPid = Nothing,
                  _msNullPacketBitrate = Nothing,
                  _msAudioBufferModel = Nothing,
                  _msTimedMetadataPid = Nothing, _msKlv = Nothing,
                  _msAudioFramesPerPes = Nothing,
                  _msPcrPeriod = Nothing, _msPcrPid = Nothing,
                  _msSegmentationMarkers = Nothing,
                  _msAribCaptionsPidControl = Nothing,
                  _msKlvDataPids = Nothing,
                  _msEbpLookaheadMs = Nothing, _msDvbSubPids = Nothing,
                  _msScte27Pids = Nothing, _msPatInterval = Nothing,
                  _msAudioStreamType = Nothing,
                  _msEsRateInPes = Nothing,
                  _msEtvPlatformPid = Nothing, _msBitrate = Nothing,
                  _msScte35Control = Nothing, _msAudioPids = Nothing,
                  _msDvbTeletextPid = Nothing, _msEbif = Nothing,
                  _msArib = Nothing, _msAribCaptionsPid = Nothing,
                  _msAbsentInputAudioBehavior = Nothing,
                  _msSegmentationTime = Nothing,
                  _msEbpAudioInterval = Nothing,
                  _msDvbNitSettings = Nothing, _msPcrControl = Nothing,
                  _msEbpPlacement = Nothing, _msRateMode = Nothing,
                  _msSegmentationStyle = Nothing,
                  _msDvbTdtSettings = Nothing}

-- | Packet Identifier (PID) for the Program Map Table (PMT) in the transport stream. Can be entered as a decimal or hexadecimal value. Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
msPmtPid :: Lens' M2tsSettings (Maybe Text)
msPmtPid = lens _msPmtPid (\ s a -> s{_msPmtPid = a})

-- | Packet Identifier (PID) for input source ETV Signal data to this output. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
msEtvSignalPid :: Lens' M2tsSettings (Maybe Text)
msEtvSignalPid = lens _msEtvSignalPid (\ s a -> s{_msEtvSignalPid = a})

-- | Packet Identifier (PID) of the elementary video stream in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
msVideoPid :: Lens' M2tsSettings (Maybe Text)
msVideoPid = lens _msVideoPid (\ s a -> s{_msVideoPid = a})

-- | If set to passthrough, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
msNielsenId3Behavior :: Lens' M2tsSettings (Maybe M2tsNielsenId3Behavior)
msNielsenId3Behavior = lens _msNielsenId3Behavior (\ s a -> s{_msNielsenId3Behavior = a})

-- | If set to multiplex, use multiplex buffer model for accurate interleaving.  Setting to bufferModel to none can lead to lower latency, but low-memory devices may not be able to play back the stream without interruptions.
msBufferModel :: Lens' M2tsSettings (Maybe M2tsBufferModel)
msBufferModel = lens _msBufferModel (\ s a -> s{_msBufferModel = a})

-- | Packet Identifier (PID) of the SCTE-35 stream in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
msScte35Pid :: Lens' M2tsSettings (Maybe Text)
msScte35Pid = lens _msScte35Pid (\ s a -> s{_msScte35Pid = a})

-- | The value of the transport stream ID field in the Program Map Table.
msTransportStreamId :: Lens' M2tsSettings (Maybe Natural)
msTransportStreamId = lens _msTransportStreamId (\ s a -> s{_msTransportStreamId = a}) . mapping _Nat

-- | The value of the program number field in the Program Map Table.
msProgramNum :: Lens' M2tsSettings (Maybe Natural)
msProgramNum = lens _msProgramNum (\ s a -> s{_msProgramNum = a}) . mapping _Nat

-- | The length in seconds of each fragment. Only used with EBP markers.
msFragmentTime :: Lens' M2tsSettings (Maybe Double)
msFragmentTime = lens _msFragmentTime (\ s a -> s{_msFragmentTime = a})

-- | When set to passthrough, timed metadata will be passed through from input to output.
msTimedMetadataBehavior :: Lens' M2tsSettings (Maybe M2tsTimedMetadataBehavior)
msTimedMetadataBehavior = lens _msTimedMetadataBehavior (\ s a -> s{_msTimedMetadataBehavior = a})

-- | When set to enabled, generates captionServiceDescriptor in PMT.
msCCDescriptor :: Lens' M2tsSettings (Maybe M2tsCCDescriptor)
msCCDescriptor = lens _msCCDescriptor (\ s a -> s{_msCCDescriptor = a})

-- | The number of milliseconds between instances of this table in the output transport stream. Valid values are 0, 10..1000.
msPmtInterval :: Lens' M2tsSettings (Maybe Natural)
msPmtInterval = lens _msPmtInterval (\ s a -> s{_msPmtInterval = a}) . mapping _Nat

-- | Inserts DVB Service Description Table (SDT) at the specified table repetition interval.
msDvbSdtSettings :: Lens' M2tsSettings (Maybe DvbSdtSettings)
msDvbSdtSettings = lens _msDvbSdtSettings (\ s a -> s{_msDvbSdtSettings = a})

-- | This field is unused and deprecated.
msEcmPid :: Lens' M2tsSettings (Maybe Text)
msEcmPid = lens _msEcmPid (\ s a -> s{_msEcmPid = a})

-- | Value in bits per second of extra null packets to insert into the transport stream. This can be used if a downstream encryption system requires periodic null packets.
msNullPacketBitrate :: Lens' M2tsSettings (Maybe Double)
msNullPacketBitrate = lens _msNullPacketBitrate (\ s a -> s{_msNullPacketBitrate = a})

-- | When set to dvb, uses DVB buffer model for Dolby Digital audio.  When set to atsc, the ATSC model is used.
msAudioBufferModel :: Lens' M2tsSettings (Maybe M2tsAudioBufferModel)
msAudioBufferModel = lens _msAudioBufferModel (\ s a -> s{_msAudioBufferModel = a})

-- | Packet Identifier (PID) of the timed metadata stream in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
msTimedMetadataPid :: Lens' M2tsSettings (Maybe Text)
msTimedMetadataPid = lens _msTimedMetadataPid (\ s a -> s{_msTimedMetadataPid = a})

-- | If set to passthrough, passes any KLV data from the input source to this output.
msKlv :: Lens' M2tsSettings (Maybe M2tsKlv)
msKlv = lens _msKlv (\ s a -> s{_msKlv = a})

-- | The number of audio frames to insert for each PES packet.
msAudioFramesPerPes :: Lens' M2tsSettings (Maybe Natural)
msAudioFramesPerPes = lens _msAudioFramesPerPes (\ s a -> s{_msAudioFramesPerPes = a}) . mapping _Nat

-- | Maximum time in milliseconds between Program Clock Reference (PCRs) inserted into the transport stream.
msPcrPeriod :: Lens' M2tsSettings (Maybe Natural)
msPcrPeriod = lens _msPcrPeriod (\ s a -> s{_msPcrPeriod = a}) . mapping _Nat

-- | Packet Identifier (PID) of the Program Clock Reference (PCR) in the transport stream. When no value is given, the encoder will assign the same value as the Video PID. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
msPcrPid :: Lens' M2tsSettings (Maybe Text)
msPcrPid = lens _msPcrPid (\ s a -> s{_msPcrPid = a})

-- | Inserts segmentation markers at each segmentationTime period. raiSegstart sets the Random Access Indicator bit in the adaptation field. raiAdapt sets the RAI bit and adds the current timecode in the private data bytes. psiSegstart inserts PAT and PMT tables at the start of segments. ebp adds Encoder Boundary Point information to the adaptation field as per OpenCable specification OC-SP-EBP-I01-130118. ebpLegacy adds Encoder Boundary Point information to the adaptation field using a legacy proprietary format.
msSegmentationMarkers :: Lens' M2tsSettings (Maybe M2tsSegmentationMarkers)
msSegmentationMarkers = lens _msSegmentationMarkers (\ s a -> s{_msSegmentationMarkers = a})

-- | If set to auto, pid number used for ARIB Captions will be auto-selected from unused pids.  If set to useConfigured, ARIB Captions will be on the configured pid number.
msAribCaptionsPidControl :: Lens' M2tsSettings (Maybe M2tsAribCaptionsPidControl)
msAribCaptionsPidControl = lens _msAribCaptionsPidControl (\ s a -> s{_msAribCaptionsPidControl = a})

-- | Packet Identifier (PID) for input source KLV data to this output. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values.  Each PID specified must be in the range of 32 (or 0x20)..8182 (or 0x1ff6).
msKlvDataPids :: Lens' M2tsSettings (Maybe Text)
msKlvDataPids = lens _msKlvDataPids (\ s a -> s{_msKlvDataPids = a})

-- | When set, enforces that Encoder Boundary Points do not come within the specified time interval of each other by looking ahead at input video. If another EBP is going to come in within the specified time interval, the current EBP is not emitted, and the segment is "stretched" to the next marker.  The lookahead value does not add latency to the system. The Live Event must be configured elsewhere to create sufficient latency to make the lookahead accurate.
msEbpLookaheadMs :: Lens' M2tsSettings (Maybe Natural)
msEbpLookaheadMs = lens _msEbpLookaheadMs (\ s a -> s{_msEbpLookaheadMs = a}) . mapping _Nat

-- | Packet Identifier (PID) for input source DVB Subtitle data to this output. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values.  Each PID specified must be in the range of 32 (or 0x20)..8182 (or 0x1ff6).
msDvbSubPids :: Lens' M2tsSettings (Maybe Text)
msDvbSubPids = lens _msDvbSubPids (\ s a -> s{_msDvbSubPids = a})

-- | Packet Identifier (PID) for input source SCTE-27 data to this output. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values.  Each PID specified must be in the range of 32 (or 0x20)..8182 (or 0x1ff6).
msScte27Pids :: Lens' M2tsSettings (Maybe Text)
msScte27Pids = lens _msScte27Pids (\ s a -> s{_msScte27Pids = a})

-- | The number of milliseconds between instances of this table in the output transport stream.  Valid values are 0, 10..1000.
msPatInterval :: Lens' M2tsSettings (Maybe Natural)
msPatInterval = lens _msPatInterval (\ s a -> s{_msPatInterval = a}) . mapping _Nat

-- | When set to atsc, uses stream type = 0x81 for AC3 and stream type = 0x87 for EAC3. When set to dvb, uses stream type = 0x06.
msAudioStreamType :: Lens' M2tsSettings (Maybe M2tsAudioStreamType)
msAudioStreamType = lens _msAudioStreamType (\ s a -> s{_msAudioStreamType = a})

-- | Include or exclude the ES Rate field in the PES header.
msEsRateInPes :: Lens' M2tsSettings (Maybe M2tsEsRateInPes)
msEsRateInPes = lens _msEsRateInPes (\ s a -> s{_msEsRateInPes = a})

-- | Packet Identifier (PID) for input source ETV Platform data to this output. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
msEtvPlatformPid :: Lens' M2tsSettings (Maybe Text)
msEtvPlatformPid = lens _msEtvPlatformPid (\ s a -> s{_msEtvPlatformPid = a})

-- | The output bitrate of the transport stream in bits per second. Setting to 0 lets the muxer automatically determine the appropriate bitrate.
msBitrate :: Lens' M2tsSettings (Maybe Natural)
msBitrate = lens _msBitrate (\ s a -> s{_msBitrate = a}) . mapping _Nat

-- | Optionally pass SCTE-35 signals from the input source to this output.
msScte35Control :: Lens' M2tsSettings (Maybe M2tsScte35Control)
msScte35Control = lens _msScte35Control (\ s a -> s{_msScte35Control = a})

-- | Packet Identifier (PID) of the elementary audio stream(s) in the transport stream. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values. Each PID specified must be in the range of 32 (or 0x20)..8182 (or 0x1ff6).
msAudioPids :: Lens' M2tsSettings (Maybe Text)
msAudioPids = lens _msAudioPids (\ s a -> s{_msAudioPids = a})

-- | Packet Identifier (PID) for input source DVB Teletext data to this output. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
msDvbTeletextPid :: Lens' M2tsSettings (Maybe Text)
msDvbTeletextPid = lens _msDvbTeletextPid (\ s a -> s{_msDvbTeletextPid = a})

-- | If set to passthrough, passes any EBIF data from the input source to this output.
msEbif :: Lens' M2tsSettings (Maybe M2tsEbifControl)
msEbif = lens _msEbif (\ s a -> s{_msEbif = a})

-- | When set to enabled, uses ARIB-compliant field muxing and removes video descriptor.
msArib :: Lens' M2tsSettings (Maybe M2tsArib)
msArib = lens _msArib (\ s a -> s{_msArib = a})

-- | Packet Identifier (PID) for ARIB Captions in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
msAribCaptionsPid :: Lens' M2tsSettings (Maybe Text)
msAribCaptionsPid = lens _msAribCaptionsPid (\ s a -> s{_msAribCaptionsPid = a})

-- | When set to drop, output audio streams will be removed from the program if the selected input audio stream is removed from the input. This allows the output audio configuration to dynamically change based on input configuration. If this is set to encodeSilence, all output audio streams will output encoded silence when not connected to an active input stream.
msAbsentInputAudioBehavior :: Lens' M2tsSettings (Maybe M2tsAbsentInputAudioBehavior)
msAbsentInputAudioBehavior = lens _msAbsentInputAudioBehavior (\ s a -> s{_msAbsentInputAudioBehavior = a})

-- | The length in seconds of each segment. Required unless markers is set to None_.
msSegmentationTime :: Lens' M2tsSettings (Maybe Double)
msSegmentationTime = lens _msSegmentationTime (\ s a -> s{_msSegmentationTime = a})

-- | When videoAndFixedIntervals is selected, audio EBP markers will be added to partitions 3 and 4. The interval between these additional markers will be fixed, and will be slightly shorter than the video EBP marker interval. Only available when EBP Cablelabs segmentation markers are selected.  Partitions 1 and 2 will always follow the video interval.
msEbpAudioInterval :: Lens' M2tsSettings (Maybe M2tsAudioInterval)
msEbpAudioInterval = lens _msEbpAudioInterval (\ s a -> s{_msEbpAudioInterval = a})

-- | Inserts DVB Network Information Table (NIT) at the specified table repetition interval.
msDvbNitSettings :: Lens' M2tsSettings (Maybe DvbNitSettings)
msDvbNitSettings = lens _msDvbNitSettings (\ s a -> s{_msDvbNitSettings = a})

-- | When set to pcrEveryPesPacket, a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.
msPcrControl :: Lens' M2tsSettings (Maybe M2tsPcrControl)
msPcrControl = lens _msPcrControl (\ s a -> s{_msPcrControl = a})

-- | Controls placement of EBP on Audio PIDs. If set to videoAndAudioPids, EBP markers will be placed on the video PID and all audio PIDs.  If set to videoPid, EBP markers will be placed on only the video PID.
msEbpPlacement :: Lens' M2tsSettings (Maybe M2tsEbpPlacement)
msEbpPlacement = lens _msEbpPlacement (\ s a -> s{_msEbpPlacement = a})

-- | When vbr, does not insert null packets into transport stream to fill specified bitrate. The bitrate setting acts as the maximum bitrate when vbr is set.
msRateMode :: Lens' M2tsSettings (Maybe M2tsRateMode)
msRateMode = lens _msRateMode (\ s a -> s{_msRateMode = a})

-- | The segmentation style parameter controls how segmentation markers are inserted into the transport stream. With avails, it is possible that segments may be truncated, which can influence where future segmentation markers are inserted. When a segmentation style of "resetCadence" is selected and a segment is truncated due to an avail, we will reset the segmentation cadence. This means the subsequent segment will have a duration of $segmentationTime seconds. When a segmentation style of "maintainCadence" is selected and a segment is truncated due to an avail, we will not reset the segmentation cadence. This means the subsequent segment will likely be truncated as well. However, all segments after that will have a duration of $segmentationTime seconds. Note that EBP lookahead is a slight exception to this rule.
msSegmentationStyle :: Lens' M2tsSettings (Maybe M2tsSegmentationStyle)
msSegmentationStyle = lens _msSegmentationStyle (\ s a -> s{_msSegmentationStyle = a})

-- | Inserts DVB Time and Date Table (TDT) at the specified table repetition interval.
msDvbTdtSettings :: Lens' M2tsSettings (Maybe DvbTdtSettings)
msDvbTdtSettings = lens _msDvbTdtSettings (\ s a -> s{_msDvbTdtSettings = a})

instance FromJSON M2tsSettings where
        parseJSON
          = withObject "M2tsSettings"
              (\ x ->
                 M2tsSettings' <$>
                   (x .:? "pmtPid") <*> (x .:? "etvSignalPid") <*>
                     (x .:? "videoPid")
                     <*> (x .:? "nielsenId3Behavior")
                     <*> (x .:? "bufferModel")
                     <*> (x .:? "scte35Pid")
                     <*> (x .:? "transportStreamId")
                     <*> (x .:? "programNum")
                     <*> (x .:? "fragmentTime")
                     <*> (x .:? "timedMetadataBehavior")
                     <*> (x .:? "ccDescriptor")
                     <*> (x .:? "pmtInterval")
                     <*> (x .:? "dvbSdtSettings")
                     <*> (x .:? "ecmPid")
                     <*> (x .:? "nullPacketBitrate")
                     <*> (x .:? "audioBufferModel")
                     <*> (x .:? "timedMetadataPid")
                     <*> (x .:? "klv")
                     <*> (x .:? "audioFramesPerPes")
                     <*> (x .:? "pcrPeriod")
                     <*> (x .:? "pcrPid")
                     <*> (x .:? "segmentationMarkers")
                     <*> (x .:? "aribCaptionsPidControl")
                     <*> (x .:? "klvDataPids")
                     <*> (x .:? "ebpLookaheadMs")
                     <*> (x .:? "dvbSubPids")
                     <*> (x .:? "scte27Pids")
                     <*> (x .:? "patInterval")
                     <*> (x .:? "audioStreamType")
                     <*> (x .:? "esRateInPes")
                     <*> (x .:? "etvPlatformPid")
                     <*> (x .:? "bitrate")
                     <*> (x .:? "scte35Control")
                     <*> (x .:? "audioPids")
                     <*> (x .:? "dvbTeletextPid")
                     <*> (x .:? "ebif")
                     <*> (x .:? "arib")
                     <*> (x .:? "aribCaptionsPid")
                     <*> (x .:? "absentInputAudioBehavior")
                     <*> (x .:? "segmentationTime")
                     <*> (x .:? "ebpAudioInterval")
                     <*> (x .:? "dvbNitSettings")
                     <*> (x .:? "pcrControl")
                     <*> (x .:? "ebpPlacement")
                     <*> (x .:? "rateMode")
                     <*> (x .:? "segmentationStyle")
                     <*> (x .:? "dvbTdtSettings"))

instance Hashable M2tsSettings where

instance NFData M2tsSettings where

instance ToJSON M2tsSettings where
        toJSON M2tsSettings'{..}
          = object
              (catMaybes
                 [("pmtPid" .=) <$> _msPmtPid,
                  ("etvSignalPid" .=) <$> _msEtvSignalPid,
                  ("videoPid" .=) <$> _msVideoPid,
                  ("nielsenId3Behavior" .=) <$> _msNielsenId3Behavior,
                  ("bufferModel" .=) <$> _msBufferModel,
                  ("scte35Pid" .=) <$> _msScte35Pid,
                  ("transportStreamId" .=) <$> _msTransportStreamId,
                  ("programNum" .=) <$> _msProgramNum,
                  ("fragmentTime" .=) <$> _msFragmentTime,
                  ("timedMetadataBehavior" .=) <$>
                    _msTimedMetadataBehavior,
                  ("ccDescriptor" .=) <$> _msCCDescriptor,
                  ("pmtInterval" .=) <$> _msPmtInterval,
                  ("dvbSdtSettings" .=) <$> _msDvbSdtSettings,
                  ("ecmPid" .=) <$> _msEcmPid,
                  ("nullPacketBitrate" .=) <$> _msNullPacketBitrate,
                  ("audioBufferModel" .=) <$> _msAudioBufferModel,
                  ("timedMetadataPid" .=) <$> _msTimedMetadataPid,
                  ("klv" .=) <$> _msKlv,
                  ("audioFramesPerPes" .=) <$> _msAudioFramesPerPes,
                  ("pcrPeriod" .=) <$> _msPcrPeriod,
                  ("pcrPid" .=) <$> _msPcrPid,
                  ("segmentationMarkers" .=) <$>
                    _msSegmentationMarkers,
                  ("aribCaptionsPidControl" .=) <$>
                    _msAribCaptionsPidControl,
                  ("klvDataPids" .=) <$> _msKlvDataPids,
                  ("ebpLookaheadMs" .=) <$> _msEbpLookaheadMs,
                  ("dvbSubPids" .=) <$> _msDvbSubPids,
                  ("scte27Pids" .=) <$> _msScte27Pids,
                  ("patInterval" .=) <$> _msPatInterval,
                  ("audioStreamType" .=) <$> _msAudioStreamType,
                  ("esRateInPes" .=) <$> _msEsRateInPes,
                  ("etvPlatformPid" .=) <$> _msEtvPlatformPid,
                  ("bitrate" .=) <$> _msBitrate,
                  ("scte35Control" .=) <$> _msScte35Control,
                  ("audioPids" .=) <$> _msAudioPids,
                  ("dvbTeletextPid" .=) <$> _msDvbTeletextPid,
                  ("ebif" .=) <$> _msEbif, ("arib" .=) <$> _msArib,
                  ("aribCaptionsPid" .=) <$> _msAribCaptionsPid,
                  ("absentInputAudioBehavior" .=) <$>
                    _msAbsentInputAudioBehavior,
                  ("segmentationTime" .=) <$> _msSegmentationTime,
                  ("ebpAudioInterval" .=) <$> _msEbpAudioInterval,
                  ("dvbNitSettings" .=) <$> _msDvbNitSettings,
                  ("pcrControl" .=) <$> _msPcrControl,
                  ("ebpPlacement" .=) <$> _msEbpPlacement,
                  ("rateMode" .=) <$> _msRateMode,
                  ("segmentationStyle" .=) <$> _msSegmentationStyle,
                  ("dvbTdtSettings" .=) <$> _msDvbTdtSettings])
