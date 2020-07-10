{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MpdSettings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.MpdSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.MpdCaptionContainerType
import Network.AWS.MediaConvert.Types.MpdScte35Esam
import Network.AWS.MediaConvert.Types.MpdScte35Source
import Network.AWS.Prelude

-- | Settings for MP4 segments in DASH
--
-- /See:/ 'mpdSettings' smart constructor.
data MpdSettings = MpdSettings'{_mpdScte35Esam ::
                                !(Maybe MpdScte35Esam),
                                _mpdScte35Source :: !(Maybe MpdScte35Source),
                                _mpdCaptionContainerType ::
                                !(Maybe MpdCaptionContainerType)}
                     deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MpdSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpdScte35Esam' - Use this setting only when you specify SCTE-35 markers from ESAM. Choose INSERT to put SCTE-35 markers in this output at the insertion points that you specify in an ESAM XML document. Provide the document in the setting SCC XML (sccXml).
--
-- * 'mpdScte35Source' - Ignore this setting unless you have SCTE-35 markers in your input video file. Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that appear in your input to also appear in this output. Choose None (NONE) if you don't want those SCTE-35 markers in this output.
--
-- * 'mpdCaptionContainerType' - Use this setting only in DASH output groups that include sidecar TTML or IMSC captions.  You specify sidecar captions in a separate output from your audio and video. Choose Raw (RAW) for captions in a single XML file in a raw container. Choose Fragmented MPEG-4 (FRAGMENTED_MP4) for captions in XML format contained within fragmented MP4 files. This set of fragmented MP4 files is separate from your video and audio fragmented MP4 files.
mpdSettings
    :: MpdSettings
mpdSettings
  = MpdSettings'{_mpdScte35Esam = Nothing,
                 _mpdScte35Source = Nothing,
                 _mpdCaptionContainerType = Nothing}

-- | Use this setting only when you specify SCTE-35 markers from ESAM. Choose INSERT to put SCTE-35 markers in this output at the insertion points that you specify in an ESAM XML document. Provide the document in the setting SCC XML (sccXml).
mpdScte35Esam :: Lens' MpdSettings (Maybe MpdScte35Esam)
mpdScte35Esam = lens _mpdScte35Esam (\ s a -> s{_mpdScte35Esam = a})

-- | Ignore this setting unless you have SCTE-35 markers in your input video file. Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that appear in your input to also appear in this output. Choose None (NONE) if you don't want those SCTE-35 markers in this output.
mpdScte35Source :: Lens' MpdSettings (Maybe MpdScte35Source)
mpdScte35Source = lens _mpdScte35Source (\ s a -> s{_mpdScte35Source = a})

-- | Use this setting only in DASH output groups that include sidecar TTML or IMSC captions.  You specify sidecar captions in a separate output from your audio and video. Choose Raw (RAW) for captions in a single XML file in a raw container. Choose Fragmented MPEG-4 (FRAGMENTED_MP4) for captions in XML format contained within fragmented MP4 files. This set of fragmented MP4 files is separate from your video and audio fragmented MP4 files.
mpdCaptionContainerType :: Lens' MpdSettings (Maybe MpdCaptionContainerType)
mpdCaptionContainerType = lens _mpdCaptionContainerType (\ s a -> s{_mpdCaptionContainerType = a})

instance FromJSON MpdSettings where
        parseJSON
          = withObject "MpdSettings"
              (\ x ->
                 MpdSettings' <$>
                   (x .:? "scte35Esam") <*> (x .:? "scte35Source") <*>
                     (x .:? "captionContainerType"))

instance Hashable MpdSettings where

instance NFData MpdSettings where

instance ToJSON MpdSettings where
        toJSON MpdSettings'{..}
          = object
              (catMaybes
                 [("scte35Esam" .=) <$> _mpdScte35Esam,
                  ("scte35Source" .=) <$> _mpdScte35Source,
                  ("captionContainerType" .=) <$>
                    _mpdCaptionContainerType])
