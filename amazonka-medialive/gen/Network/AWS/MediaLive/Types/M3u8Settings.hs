{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M3u8Settings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.M3u8Settings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.M3u8NielsenId3Behavior
import Network.AWS.MediaLive.Types.M3u8PcrControl
import Network.AWS.MediaLive.Types.M3u8Scte35Behavior
import Network.AWS.MediaLive.Types.M3u8TimedMetadataBehavior
import Network.AWS.Prelude

-- | Settings information for the .m3u8 container
--
-- /See:/ 'm3u8Settings' smart constructor.
data M3u8Settings = M3u8Settings'{_mPmtPid ::
                                  !(Maybe Text),
                                  _mVideoPid :: !(Maybe Text),
                                  _mNielsenId3Behavior ::
                                  !(Maybe M3u8NielsenId3Behavior),
                                  _mScte35Pid :: !(Maybe Text),
                                  _mTransportStreamId :: !(Maybe Nat),
                                  _mProgramNum :: !(Maybe Nat),
                                  _mTimedMetadataBehavior ::
                                  !(Maybe M3u8TimedMetadataBehavior),
                                  _mPmtInterval :: !(Maybe Nat),
                                  _mEcmPid :: !(Maybe Text),
                                  _mTimedMetadataPid :: !(Maybe Text),
                                  _mAudioFramesPerPes :: !(Maybe Nat),
                                  _mPcrPeriod :: !(Maybe Nat),
                                  _mPcrPid :: !(Maybe Text),
                                  _mPatInterval :: !(Maybe Nat),
                                  _mAudioPids :: !(Maybe Text),
                                  _mScte35Behavior ::
                                  !(Maybe M3u8Scte35Behavior),
                                  _mPcrControl :: !(Maybe M3u8PcrControl)}
                      deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'M3u8Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mPmtPid' - Packet Identifier (PID) for the Program Map Table (PMT) in the transport stream. Can be entered as a decimal or hexadecimal value.
--
-- * 'mVideoPid' - Packet Identifier (PID) of the elementary video stream in the transport stream. Can be entered as a decimal or hexadecimal value.
--
-- * 'mNielsenId3Behavior' - If set to passthrough, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
--
-- * 'mScte35Pid' - Packet Identifier (PID) of the SCTE-35 stream in the transport stream. Can be entered as a decimal or hexadecimal value.
--
-- * 'mTransportStreamId' - The value of the transport stream ID field in the Program Map Table.
--
-- * 'mProgramNum' - The value of the program number field in the Program Map Table.
--
-- * 'mTimedMetadataBehavior' - When set to passthrough, timed metadata is passed through from input to output.
--
-- * 'mPmtInterval' - The number of milliseconds between instances of this table in the output transport stream. A value of \"0\" writes out the PMT once per segment file.
--
-- * 'mEcmPid' - This parameter is unused and deprecated.
--
-- * 'mTimedMetadataPid' - Packet Identifier (PID) of the timed metadata stream in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- * 'mAudioFramesPerPes' - The number of audio frames to insert for each PES packet.
--
-- * 'mPcrPeriod' - Maximum time in milliseconds between Program Clock References (PCRs) inserted into the transport stream.
--
-- * 'mPcrPid' - Packet Identifier (PID) of the Program Clock Reference (PCR) in the transport stream. When no value is given, the encoder will assign the same value as the Video PID. Can be entered as a decimal or hexadecimal value.
--
-- * 'mPatInterval' - The number of milliseconds between instances of this table in the output transport stream. A value of \"0\" writes out the PMT once per segment file.
--
-- * 'mAudioPids' - Packet Identifier (PID) of the elementary audio stream(s) in the transport stream. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values.
--
-- * 'mScte35Behavior' - If set to passthrough, passes any SCTE-35 signals from the input source to this output.
--
-- * 'mPcrControl' - When set to pcrEveryPesPacket, a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.
m3u8Settings
    :: M3u8Settings
m3u8Settings
  = M3u8Settings'{_mPmtPid = Nothing,
                  _mVideoPid = Nothing, _mNielsenId3Behavior = Nothing,
                  _mScte35Pid = Nothing, _mTransportStreamId = Nothing,
                  _mProgramNum = Nothing,
                  _mTimedMetadataBehavior = Nothing,
                  _mPmtInterval = Nothing, _mEcmPid = Nothing,
                  _mTimedMetadataPid = Nothing,
                  _mAudioFramesPerPes = Nothing, _mPcrPeriod = Nothing,
                  _mPcrPid = Nothing, _mPatInterval = Nothing,
                  _mAudioPids = Nothing, _mScte35Behavior = Nothing,
                  _mPcrControl = Nothing}

-- | Packet Identifier (PID) for the Program Map Table (PMT) in the transport stream. Can be entered as a decimal or hexadecimal value.
mPmtPid :: Lens' M3u8Settings (Maybe Text)
mPmtPid = lens _mPmtPid (\ s a -> s{_mPmtPid = a})

-- | Packet Identifier (PID) of the elementary video stream in the transport stream. Can be entered as a decimal or hexadecimal value.
mVideoPid :: Lens' M3u8Settings (Maybe Text)
mVideoPid = lens _mVideoPid (\ s a -> s{_mVideoPid = a})

-- | If set to passthrough, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
mNielsenId3Behavior :: Lens' M3u8Settings (Maybe M3u8NielsenId3Behavior)
mNielsenId3Behavior = lens _mNielsenId3Behavior (\ s a -> s{_mNielsenId3Behavior = a})

-- | Packet Identifier (PID) of the SCTE-35 stream in the transport stream. Can be entered as a decimal or hexadecimal value.
mScte35Pid :: Lens' M3u8Settings (Maybe Text)
mScte35Pid = lens _mScte35Pid (\ s a -> s{_mScte35Pid = a})

-- | The value of the transport stream ID field in the Program Map Table.
mTransportStreamId :: Lens' M3u8Settings (Maybe Natural)
mTransportStreamId = lens _mTransportStreamId (\ s a -> s{_mTransportStreamId = a}) . mapping _Nat

-- | The value of the program number field in the Program Map Table.
mProgramNum :: Lens' M3u8Settings (Maybe Natural)
mProgramNum = lens _mProgramNum (\ s a -> s{_mProgramNum = a}) . mapping _Nat

-- | When set to passthrough, timed metadata is passed through from input to output.
mTimedMetadataBehavior :: Lens' M3u8Settings (Maybe M3u8TimedMetadataBehavior)
mTimedMetadataBehavior = lens _mTimedMetadataBehavior (\ s a -> s{_mTimedMetadataBehavior = a})

-- | The number of milliseconds between instances of this table in the output transport stream. A value of \"0\" writes out the PMT once per segment file.
mPmtInterval :: Lens' M3u8Settings (Maybe Natural)
mPmtInterval = lens _mPmtInterval (\ s a -> s{_mPmtInterval = a}) . mapping _Nat

-- | This parameter is unused and deprecated.
mEcmPid :: Lens' M3u8Settings (Maybe Text)
mEcmPid = lens _mEcmPid (\ s a -> s{_mEcmPid = a})

-- | Packet Identifier (PID) of the timed metadata stream in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
mTimedMetadataPid :: Lens' M3u8Settings (Maybe Text)
mTimedMetadataPid = lens _mTimedMetadataPid (\ s a -> s{_mTimedMetadataPid = a})

-- | The number of audio frames to insert for each PES packet.
mAudioFramesPerPes :: Lens' M3u8Settings (Maybe Natural)
mAudioFramesPerPes = lens _mAudioFramesPerPes (\ s a -> s{_mAudioFramesPerPes = a}) . mapping _Nat

-- | Maximum time in milliseconds between Program Clock References (PCRs) inserted into the transport stream.
mPcrPeriod :: Lens' M3u8Settings (Maybe Natural)
mPcrPeriod = lens _mPcrPeriod (\ s a -> s{_mPcrPeriod = a}) . mapping _Nat

-- | Packet Identifier (PID) of the Program Clock Reference (PCR) in the transport stream. When no value is given, the encoder will assign the same value as the Video PID. Can be entered as a decimal or hexadecimal value.
mPcrPid :: Lens' M3u8Settings (Maybe Text)
mPcrPid = lens _mPcrPid (\ s a -> s{_mPcrPid = a})

-- | The number of milliseconds between instances of this table in the output transport stream. A value of \"0\" writes out the PMT once per segment file.
mPatInterval :: Lens' M3u8Settings (Maybe Natural)
mPatInterval = lens _mPatInterval (\ s a -> s{_mPatInterval = a}) . mapping _Nat

-- | Packet Identifier (PID) of the elementary audio stream(s) in the transport stream. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values.
mAudioPids :: Lens' M3u8Settings (Maybe Text)
mAudioPids = lens _mAudioPids (\ s a -> s{_mAudioPids = a})

-- | If set to passthrough, passes any SCTE-35 signals from the input source to this output.
mScte35Behavior :: Lens' M3u8Settings (Maybe M3u8Scte35Behavior)
mScte35Behavior = lens _mScte35Behavior (\ s a -> s{_mScte35Behavior = a})

-- | When set to pcrEveryPesPacket, a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.
mPcrControl :: Lens' M3u8Settings (Maybe M3u8PcrControl)
mPcrControl = lens _mPcrControl (\ s a -> s{_mPcrControl = a})

instance FromJSON M3u8Settings where
        parseJSON
          = withObject "M3u8Settings"
              (\ x ->
                 M3u8Settings' <$>
                   (x .:? "pmtPid") <*> (x .:? "videoPid") <*>
                     (x .:? "nielsenId3Behavior")
                     <*> (x .:? "scte35Pid")
                     <*> (x .:? "transportStreamId")
                     <*> (x .:? "programNum")
                     <*> (x .:? "timedMetadataBehavior")
                     <*> (x .:? "pmtInterval")
                     <*> (x .:? "ecmPid")
                     <*> (x .:? "timedMetadataPid")
                     <*> (x .:? "audioFramesPerPes")
                     <*> (x .:? "pcrPeriod")
                     <*> (x .:? "pcrPid")
                     <*> (x .:? "patInterval")
                     <*> (x .:? "audioPids")
                     <*> (x .:? "scte35Behavior")
                     <*> (x .:? "pcrControl"))

instance Hashable M3u8Settings where

instance NFData M3u8Settings where

instance ToJSON M3u8Settings where
        toJSON M3u8Settings'{..}
          = object
              (catMaybes
                 [("pmtPid" .=) <$> _mPmtPid,
                  ("videoPid" .=) <$> _mVideoPid,
                  ("nielsenId3Behavior" .=) <$> _mNielsenId3Behavior,
                  ("scte35Pid" .=) <$> _mScte35Pid,
                  ("transportStreamId" .=) <$> _mTransportStreamId,
                  ("programNum" .=) <$> _mProgramNum,
                  ("timedMetadataBehavior" .=) <$>
                    _mTimedMetadataBehavior,
                  ("pmtInterval" .=) <$> _mPmtInterval,
                  ("ecmPid" .=) <$> _mEcmPid,
                  ("timedMetadataPid" .=) <$> _mTimedMetadataPid,
                  ("audioFramesPerPes" .=) <$> _mAudioFramesPerPes,
                  ("pcrPeriod" .=) <$> _mPcrPeriod,
                  ("pcrPid" .=) <$> _mPcrPid,
                  ("patInterval" .=) <$> _mPatInterval,
                  ("audioPids" .=) <$> _mAudioPids,
                  ("scte35Behavior" .=) <$> _mScte35Behavior,
                  ("pcrControl" .=) <$> _mPcrControl])
