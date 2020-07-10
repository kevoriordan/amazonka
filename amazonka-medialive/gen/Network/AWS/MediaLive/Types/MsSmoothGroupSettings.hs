{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MsSmoothGroupSettings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.MsSmoothGroupSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.InputLossActionForMsSmoothOut
import Network.AWS.MediaLive.Types.OutputLocationRef
import Network.AWS.MediaLive.Types.SmoothGroupAudioOnlyTimecodeControl
import Network.AWS.MediaLive.Types.SmoothGroupCertificateMode
import Network.AWS.MediaLive.Types.SmoothGroupEventIdMode
import Network.AWS.MediaLive.Types.SmoothGroupEventStopBehavior
import Network.AWS.MediaLive.Types.SmoothGroupSegmentationMode
import Network.AWS.MediaLive.Types.SmoothGroupSparseTrackType
import Network.AWS.MediaLive.Types.SmoothGroupStreamManifestBehavior
import Network.AWS.MediaLive.Types.SmoothGroupTimestampOffsetMode
import Network.AWS.Prelude

-- | Ms Smooth Group Settings
--
-- /See:/ 'msSmoothGroupSettings' smart constructor.
data MsSmoothGroupSettings = MsSmoothGroupSettings'{_msgsFragmentLength
                                                    :: !(Maybe Nat),
                                                    _msgsStreamManifestBehavior
                                                    ::
                                                    !(Maybe
                                                        SmoothGroupStreamManifestBehavior),
                                                    _msgsSendDelayMs ::
                                                    !(Maybe Nat),
                                                    _msgsEventStopBehavior ::
                                                    !(Maybe
                                                        SmoothGroupEventStopBehavior),
                                                    _msgsTimestampOffsetMode ::
                                                    !(Maybe
                                                        SmoothGroupTimestampOffsetMode),
                                                    _msgsNumRetries ::
                                                    !(Maybe Nat),
                                                    _msgsAcquisitionPointId ::
                                                    !(Maybe Text),
                                                    _msgsInputLossAction ::
                                                    !(Maybe
                                                        InputLossActionForMsSmoothOut),
                                                    _msgsTimestampOffset ::
                                                    !(Maybe Text),
                                                    _msgsCertificateMode ::
                                                    !(Maybe
                                                        SmoothGroupCertificateMode),
                                                    _msgsSparseTrackType ::
                                                    !(Maybe
                                                        SmoothGroupSparseTrackType),
                                                    _msgsConnectionRetryInterval
                                                    :: !(Maybe Nat),
                                                    _msgsFilecacheDuration ::
                                                    !(Maybe Nat),
                                                    _msgsRestartDelay ::
                                                    !(Maybe Nat),
                                                    _msgsEventIdMode ::
                                                    !(Maybe
                                                        SmoothGroupEventIdMode),
                                                    _msgsAudioOnlyTimecodeControl
                                                    ::
                                                    !(Maybe
                                                        SmoothGroupAudioOnlyTimecodeControl),
                                                    _msgsSegmentationMode ::
                                                    !(Maybe
                                                        SmoothGroupSegmentationMode),
                                                    _msgsEventId ::
                                                    !(Maybe Text),
                                                    _msgsDestination ::
                                                    !OutputLocationRef}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'MsSmoothGroupSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msgsFragmentLength' - Length of mp4 fragments to generate (in seconds). Fragment length must be compatible with GOP size and framerate.
--
-- * 'msgsStreamManifestBehavior' - When set to send, send stream manifest so publishing point doesn't start until all streams start.
--
-- * 'msgsSendDelayMs' - Number of milliseconds to delay the output from the second pipeline.
--
-- * 'msgsEventStopBehavior' - When set to sendEos, send EOS signal to IIS server when stopping the event
--
-- * 'msgsTimestampOffsetMode' - Type of timestamp date offset to use. - useEventStartDate: Use the date the event was started as the offset - useConfiguredOffset: Use an explicitly configured date as the offset
--
-- * 'msgsNumRetries' - Number of retry attempts.
--
-- * 'msgsAcquisitionPointId' - The value of the "Acquisition Point Identity" element used in each message placed in the sparse track.  Only enabled if sparseTrackType is not "none".
--
-- * 'msgsInputLossAction' - Parameter that control output group behavior on input loss.
--
-- * 'msgsTimestampOffset' - Timestamp offset for the event.  Only used if timestampOffsetMode is set to useConfiguredOffset.
--
-- * 'msgsCertificateMode' - If set to verifyAuthenticity, verify the https certificate chain to a trusted Certificate Authority (CA).  This will cause https outputs to self-signed certificates to fail.
--
-- * 'msgsSparseTrackType' - If set to scte35, use incoming SCTE-35 messages to generate a sparse track in this group of MS-Smooth outputs. scte35WithoutSegmentation is the same as scte35, except EML will not start a new segment at a SCTE-35 marker. It will still encode an IDR frame at a SCTE-35 marker.
--
-- * 'msgsConnectionRetryInterval' - Number of seconds to wait before retrying connection to the IIS server if the connection is lost. Content will be cached during this time and the cache will be be delivered to the IIS server once the connection is re-established.
--
-- * 'msgsFilecacheDuration' - Size in seconds of file cache for streaming outputs.
--
-- * 'msgsRestartDelay' - Number of seconds before initiating a restart due to output failure, due to exhausting the numRetries on one segment, or exceeding filecacheDuration.
--
-- * 'msgsEventIdMode' - Specifies whether or not to send an event ID to the IIS server. If no event ID is sent and the same Live Event is used without changing the publishing point, clients might see cached video from the previous run. Options: - "useConfigured" - use the value provided in eventId - "useTimestamp" - generate and send an event ID based on the current timestamp - "noEventId" - do not send an event ID to the IIS server.
--
-- * 'msgsAudioOnlyTimecodeControl' - If set to passthrough for an audio-only MS Smooth output, the fragment absolute time will be set to the current timecode. This option does not write timecodes to the audio elementary stream.
--
-- * 'msgsSegmentationMode' - useInputSegmentation has been deprecated. The configured segment size is always used.
--
-- * 'msgsEventId' - MS Smooth event ID to be sent to the IIS server. Should only be specified if eventIdMode is set to useConfigured.
--
-- * 'msgsDestination' - Smooth Streaming publish point on an IIS server. Elemental Live acts as a "Push" encoder to IIS.
msSmoothGroupSettings
    :: OutputLocationRef -- ^ 'msgsDestination'
    -> MsSmoothGroupSettings
msSmoothGroupSettings pDestination_
  = MsSmoothGroupSettings'{_msgsFragmentLength =
                             Nothing,
                           _msgsStreamManifestBehavior = Nothing,
                           _msgsSendDelayMs = Nothing,
                           _msgsEventStopBehavior = Nothing,
                           _msgsTimestampOffsetMode = Nothing,
                           _msgsNumRetries = Nothing,
                           _msgsAcquisitionPointId = Nothing,
                           _msgsInputLossAction = Nothing,
                           _msgsTimestampOffset = Nothing,
                           _msgsCertificateMode = Nothing,
                           _msgsSparseTrackType = Nothing,
                           _msgsConnectionRetryInterval = Nothing,
                           _msgsFilecacheDuration = Nothing,
                           _msgsRestartDelay = Nothing,
                           _msgsEventIdMode = Nothing,
                           _msgsAudioOnlyTimecodeControl = Nothing,
                           _msgsSegmentationMode = Nothing,
                           _msgsEventId = Nothing,
                           _msgsDestination = pDestination_}

-- | Length of mp4 fragments to generate (in seconds). Fragment length must be compatible with GOP size and framerate.
msgsFragmentLength :: Lens' MsSmoothGroupSettings (Maybe Natural)
msgsFragmentLength = lens _msgsFragmentLength (\ s a -> s{_msgsFragmentLength = a}) . mapping _Nat

-- | When set to send, send stream manifest so publishing point doesn't start until all streams start.
msgsStreamManifestBehavior :: Lens' MsSmoothGroupSettings (Maybe SmoothGroupStreamManifestBehavior)
msgsStreamManifestBehavior = lens _msgsStreamManifestBehavior (\ s a -> s{_msgsStreamManifestBehavior = a})

-- | Number of milliseconds to delay the output from the second pipeline.
msgsSendDelayMs :: Lens' MsSmoothGroupSettings (Maybe Natural)
msgsSendDelayMs = lens _msgsSendDelayMs (\ s a -> s{_msgsSendDelayMs = a}) . mapping _Nat

-- | When set to sendEos, send EOS signal to IIS server when stopping the event
msgsEventStopBehavior :: Lens' MsSmoothGroupSettings (Maybe SmoothGroupEventStopBehavior)
msgsEventStopBehavior = lens _msgsEventStopBehavior (\ s a -> s{_msgsEventStopBehavior = a})

-- | Type of timestamp date offset to use. - useEventStartDate: Use the date the event was started as the offset - useConfiguredOffset: Use an explicitly configured date as the offset
msgsTimestampOffsetMode :: Lens' MsSmoothGroupSettings (Maybe SmoothGroupTimestampOffsetMode)
msgsTimestampOffsetMode = lens _msgsTimestampOffsetMode (\ s a -> s{_msgsTimestampOffsetMode = a})

-- | Number of retry attempts.
msgsNumRetries :: Lens' MsSmoothGroupSettings (Maybe Natural)
msgsNumRetries = lens _msgsNumRetries (\ s a -> s{_msgsNumRetries = a}) . mapping _Nat

-- | The value of the "Acquisition Point Identity" element used in each message placed in the sparse track.  Only enabled if sparseTrackType is not "none".
msgsAcquisitionPointId :: Lens' MsSmoothGroupSettings (Maybe Text)
msgsAcquisitionPointId = lens _msgsAcquisitionPointId (\ s a -> s{_msgsAcquisitionPointId = a})

-- | Parameter that control output group behavior on input loss.
msgsInputLossAction :: Lens' MsSmoothGroupSettings (Maybe InputLossActionForMsSmoothOut)
msgsInputLossAction = lens _msgsInputLossAction (\ s a -> s{_msgsInputLossAction = a})

-- | Timestamp offset for the event.  Only used if timestampOffsetMode is set to useConfiguredOffset.
msgsTimestampOffset :: Lens' MsSmoothGroupSettings (Maybe Text)
msgsTimestampOffset = lens _msgsTimestampOffset (\ s a -> s{_msgsTimestampOffset = a})

-- | If set to verifyAuthenticity, verify the https certificate chain to a trusted Certificate Authority (CA).  This will cause https outputs to self-signed certificates to fail.
msgsCertificateMode :: Lens' MsSmoothGroupSettings (Maybe SmoothGroupCertificateMode)
msgsCertificateMode = lens _msgsCertificateMode (\ s a -> s{_msgsCertificateMode = a})

-- | If set to scte35, use incoming SCTE-35 messages to generate a sparse track in this group of MS-Smooth outputs. scte35WithoutSegmentation is the same as scte35, except EML will not start a new segment at a SCTE-35 marker. It will still encode an IDR frame at a SCTE-35 marker.
msgsSparseTrackType :: Lens' MsSmoothGroupSettings (Maybe SmoothGroupSparseTrackType)
msgsSparseTrackType = lens _msgsSparseTrackType (\ s a -> s{_msgsSparseTrackType = a})

-- | Number of seconds to wait before retrying connection to the IIS server if the connection is lost. Content will be cached during this time and the cache will be be delivered to the IIS server once the connection is re-established.
msgsConnectionRetryInterval :: Lens' MsSmoothGroupSettings (Maybe Natural)
msgsConnectionRetryInterval = lens _msgsConnectionRetryInterval (\ s a -> s{_msgsConnectionRetryInterval = a}) . mapping _Nat

-- | Size in seconds of file cache for streaming outputs.
msgsFilecacheDuration :: Lens' MsSmoothGroupSettings (Maybe Natural)
msgsFilecacheDuration = lens _msgsFilecacheDuration (\ s a -> s{_msgsFilecacheDuration = a}) . mapping _Nat

-- | Number of seconds before initiating a restart due to output failure, due to exhausting the numRetries on one segment, or exceeding filecacheDuration.
msgsRestartDelay :: Lens' MsSmoothGroupSettings (Maybe Natural)
msgsRestartDelay = lens _msgsRestartDelay (\ s a -> s{_msgsRestartDelay = a}) . mapping _Nat

-- | Specifies whether or not to send an event ID to the IIS server. If no event ID is sent and the same Live Event is used without changing the publishing point, clients might see cached video from the previous run. Options: - "useConfigured" - use the value provided in eventId - "useTimestamp" - generate and send an event ID based on the current timestamp - "noEventId" - do not send an event ID to the IIS server.
msgsEventIdMode :: Lens' MsSmoothGroupSettings (Maybe SmoothGroupEventIdMode)
msgsEventIdMode = lens _msgsEventIdMode (\ s a -> s{_msgsEventIdMode = a})

-- | If set to passthrough for an audio-only MS Smooth output, the fragment absolute time will be set to the current timecode. This option does not write timecodes to the audio elementary stream.
msgsAudioOnlyTimecodeControl :: Lens' MsSmoothGroupSettings (Maybe SmoothGroupAudioOnlyTimecodeControl)
msgsAudioOnlyTimecodeControl = lens _msgsAudioOnlyTimecodeControl (\ s a -> s{_msgsAudioOnlyTimecodeControl = a})

-- | useInputSegmentation has been deprecated. The configured segment size is always used.
msgsSegmentationMode :: Lens' MsSmoothGroupSettings (Maybe SmoothGroupSegmentationMode)
msgsSegmentationMode = lens _msgsSegmentationMode (\ s a -> s{_msgsSegmentationMode = a})

-- | MS Smooth event ID to be sent to the IIS server. Should only be specified if eventIdMode is set to useConfigured.
msgsEventId :: Lens' MsSmoothGroupSettings (Maybe Text)
msgsEventId = lens _msgsEventId (\ s a -> s{_msgsEventId = a})

-- | Smooth Streaming publish point on an IIS server. Elemental Live acts as a "Push" encoder to IIS.
msgsDestination :: Lens' MsSmoothGroupSettings OutputLocationRef
msgsDestination = lens _msgsDestination (\ s a -> s{_msgsDestination = a})

instance FromJSON MsSmoothGroupSettings where
        parseJSON
          = withObject "MsSmoothGroupSettings"
              (\ x ->
                 MsSmoothGroupSettings' <$>
                   (x .:? "fragmentLength") <*>
                     (x .:? "streamManifestBehavior")
                     <*> (x .:? "sendDelayMs")
                     <*> (x .:? "eventStopBehavior")
                     <*> (x .:? "timestampOffsetMode")
                     <*> (x .:? "numRetries")
                     <*> (x .:? "acquisitionPointId")
                     <*> (x .:? "inputLossAction")
                     <*> (x .:? "timestampOffset")
                     <*> (x .:? "certificateMode")
                     <*> (x .:? "sparseTrackType")
                     <*> (x .:? "connectionRetryInterval")
                     <*> (x .:? "filecacheDuration")
                     <*> (x .:? "restartDelay")
                     <*> (x .:? "eventIdMode")
                     <*> (x .:? "audioOnlyTimecodeControl")
                     <*> (x .:? "segmentationMode")
                     <*> (x .:? "eventId")
                     <*> (x .: "destination"))

instance Hashable MsSmoothGroupSettings where

instance NFData MsSmoothGroupSettings where

instance ToJSON MsSmoothGroupSettings where
        toJSON MsSmoothGroupSettings'{..}
          = object
              (catMaybes
                 [("fragmentLength" .=) <$> _msgsFragmentLength,
                  ("streamManifestBehavior" .=) <$>
                    _msgsStreamManifestBehavior,
                  ("sendDelayMs" .=) <$> _msgsSendDelayMs,
                  ("eventStopBehavior" .=) <$> _msgsEventStopBehavior,
                  ("timestampOffsetMode" .=) <$>
                    _msgsTimestampOffsetMode,
                  ("numRetries" .=) <$> _msgsNumRetries,
                  ("acquisitionPointId" .=) <$>
                    _msgsAcquisitionPointId,
                  ("inputLossAction" .=) <$> _msgsInputLossAction,
                  ("timestampOffset" .=) <$> _msgsTimestampOffset,
                  ("certificateMode" .=) <$> _msgsCertificateMode,
                  ("sparseTrackType" .=) <$> _msgsSparseTrackType,
                  ("connectionRetryInterval" .=) <$>
                    _msgsConnectionRetryInterval,
                  ("filecacheDuration" .=) <$> _msgsFilecacheDuration,
                  ("restartDelay" .=) <$> _msgsRestartDelay,
                  ("eventIdMode" .=) <$> _msgsEventIdMode,
                  ("audioOnlyTimecodeControl" .=) <$>
                    _msgsAudioOnlyTimecodeControl,
                  ("segmentationMode" .=) <$> _msgsSegmentationMode,
                  ("eventId" .=) <$> _msgsEventId,
                  Just ("destination" .= _msgsDestination)])
