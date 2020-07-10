{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticTranscoder.Types.Product (
    module Network.AWS.ElasticTranscoder.Types.Artwork,
    module Network.AWS.ElasticTranscoder.Types.AudioCodecOptions,
    module Network.AWS.ElasticTranscoder.Types.AudioParameters,
    module Network.AWS.ElasticTranscoder.Types.CaptionFormat,
    module Network.AWS.ElasticTranscoder.Types.CaptionSource,
    module Network.AWS.ElasticTranscoder.Types.Captions,
    module Network.AWS.ElasticTranscoder.Types.Clip,
    module Network.AWS.ElasticTranscoder.Types.CreateJobOutput,
    module Network.AWS.ElasticTranscoder.Types.CreateJobPlaylist,
    module Network.AWS.ElasticTranscoder.Types.DetectedProperties,
    module Network.AWS.ElasticTranscoder.Types.Encryption,
    module Network.AWS.ElasticTranscoder.Types.HlsContentProtection,
    module Network.AWS.ElasticTranscoder.Types.InputCaptions,
    module Network.AWS.ElasticTranscoder.Types.Job',
    module Network.AWS.ElasticTranscoder.Types.JobAlbumArt,
    module Network.AWS.ElasticTranscoder.Types.JobInput,
    module Network.AWS.ElasticTranscoder.Types.JobOutput,
    module Network.AWS.ElasticTranscoder.Types.JobWatermark,
    module Network.AWS.ElasticTranscoder.Types.Notifications,
    module Network.AWS.ElasticTranscoder.Types.Permission,
    module Network.AWS.ElasticTranscoder.Types.Pipeline,
    module Network.AWS.ElasticTranscoder.Types.PipelineOutputConfig,
    module Network.AWS.ElasticTranscoder.Types.PlayReadyDrm,
    module Network.AWS.ElasticTranscoder.Types.Playlist,
    module Network.AWS.ElasticTranscoder.Types.Preset,
    module Network.AWS.ElasticTranscoder.Types.PresetWatermark,
    module Network.AWS.ElasticTranscoder.Types.Thumbnails,
    module Network.AWS.ElasticTranscoder.Types.TimeSpan,
    module Network.AWS.ElasticTranscoder.Types.Timing,
    module Network.AWS.ElasticTranscoder.Types.VideoParameters,
    module Network.AWS.ElasticTranscoder.Types.Warning
  ) where

import Network.AWS.ElasticTranscoder.Types.Artwork
import Network.AWS.ElasticTranscoder.Types.AudioCodecOptions
import Network.AWS.ElasticTranscoder.Types.AudioParameters
import Network.AWS.ElasticTranscoder.Types.CaptionFormat
import Network.AWS.ElasticTranscoder.Types.CaptionSource
import Network.AWS.ElasticTranscoder.Types.Captions
import Network.AWS.ElasticTranscoder.Types.Clip
import Network.AWS.ElasticTranscoder.Types.CreateJobOutput
import Network.AWS.ElasticTranscoder.Types.CreateJobPlaylist
import Network.AWS.ElasticTranscoder.Types.DetectedProperties
import Network.AWS.ElasticTranscoder.Types.Encryption
import Network.AWS.ElasticTranscoder.Types.HlsContentProtection
import Network.AWS.ElasticTranscoder.Types.InputCaptions
import Network.AWS.ElasticTranscoder.Types.Job'
import Network.AWS.ElasticTranscoder.Types.JobAlbumArt
import Network.AWS.ElasticTranscoder.Types.JobInput
import Network.AWS.ElasticTranscoder.Types.JobOutput
import Network.AWS.ElasticTranscoder.Types.JobWatermark
import Network.AWS.ElasticTranscoder.Types.Notifications
import Network.AWS.ElasticTranscoder.Types.Permission
import Network.AWS.ElasticTranscoder.Types.Pipeline
import Network.AWS.ElasticTranscoder.Types.PipelineOutputConfig
import Network.AWS.ElasticTranscoder.Types.PlayReadyDrm
import Network.AWS.ElasticTranscoder.Types.Playlist
import Network.AWS.ElasticTranscoder.Types.Preset
import Network.AWS.ElasticTranscoder.Types.PresetWatermark
import Network.AWS.ElasticTranscoder.Types.Thumbnails
import Network.AWS.ElasticTranscoder.Types.TimeSpan
import Network.AWS.ElasticTranscoder.Types.Timing
import Network.AWS.ElasticTranscoder.Types.VideoParameters
import Network.AWS.ElasticTranscoder.Types.Warning
import Network.AWS.Lens
import Network.AWS.Prelude
