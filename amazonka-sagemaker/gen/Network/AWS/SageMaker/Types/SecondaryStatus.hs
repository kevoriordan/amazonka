{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SecondaryStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.SecondaryStatus (
  SecondaryStatus (
    ..
    , SCompleted
    , SDownloading
    , SDownloadingTrainingImage
    , SFailed
    , SInterrupted
    , SLaunchingMLInstances
    , SMaxRuntimeExceeded
    , SMaxWaitTimeExceeded
    , SPreparingTrainingStack
    , SStarting
    , SStopped
    , SStopping
    , STraining
    , SUploading
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SecondaryStatus = SecondaryStatus' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern SCompleted :: SecondaryStatus
pattern SCompleted = SecondaryStatus' "Completed"

pattern SDownloading :: SecondaryStatus
pattern SDownloading = SecondaryStatus' "Downloading"

pattern SDownloadingTrainingImage :: SecondaryStatus
pattern SDownloadingTrainingImage = SecondaryStatus' "DownloadingTrainingImage"

pattern SFailed :: SecondaryStatus
pattern SFailed = SecondaryStatus' "Failed"

pattern SInterrupted :: SecondaryStatus
pattern SInterrupted = SecondaryStatus' "Interrupted"

pattern SLaunchingMLInstances :: SecondaryStatus
pattern SLaunchingMLInstances = SecondaryStatus' "LaunchingMLInstances"

pattern SMaxRuntimeExceeded :: SecondaryStatus
pattern SMaxRuntimeExceeded = SecondaryStatus' "MaxRuntimeExceeded"

pattern SMaxWaitTimeExceeded :: SecondaryStatus
pattern SMaxWaitTimeExceeded = SecondaryStatus' "MaxWaitTimeExceeded"

pattern SPreparingTrainingStack :: SecondaryStatus
pattern SPreparingTrainingStack = SecondaryStatus' "PreparingTrainingStack"

pattern SStarting :: SecondaryStatus
pattern SStarting = SecondaryStatus' "Starting"

pattern SStopped :: SecondaryStatus
pattern SStopped = SecondaryStatus' "Stopped"

pattern SStopping :: SecondaryStatus
pattern SStopping = SecondaryStatus' "Stopping"

pattern STraining :: SecondaryStatus
pattern STraining = SecondaryStatus' "Training"

pattern SUploading :: SecondaryStatus
pattern SUploading = SecondaryStatus' "Uploading"

{-# COMPLETE
  SCompleted,
  SDownloading,
  SDownloadingTrainingImage,
  SFailed,
  SInterrupted,
  SLaunchingMLInstances,
  SMaxRuntimeExceeded,
  SMaxWaitTimeExceeded,
  SPreparingTrainingStack,
  SStarting,
  SStopped,
  SStopping,
  STraining,
  SUploading,
  SecondaryStatus' #-}

instance FromText SecondaryStatus where
    parser = (SecondaryStatus' . mk) <$> takeText

instance ToText SecondaryStatus where
    toText (SecondaryStatus' ci) = original ci

-- | Represents an enum of /known/ $SecondaryStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SecondaryStatus where
    toEnum i = case i of
        0 -> SCompleted
        1 -> SDownloading
        2 -> SDownloadingTrainingImage
        3 -> SFailed
        4 -> SInterrupted
        5 -> SLaunchingMLInstances
        6 -> SMaxRuntimeExceeded
        7 -> SMaxWaitTimeExceeded
        8 -> SPreparingTrainingStack
        9 -> SStarting
        10 -> SStopped
        11 -> SStopping
        12 -> STraining
        13 -> SUploading
        _ -> (error . showText) $ "Unknown index for SecondaryStatus: " <> toText i
    fromEnum x = case x of
        SCompleted -> 0
        SDownloading -> 1
        SDownloadingTrainingImage -> 2
        SFailed -> 3
        SInterrupted -> 4
        SLaunchingMLInstances -> 5
        SMaxRuntimeExceeded -> 6
        SMaxWaitTimeExceeded -> 7
        SPreparingTrainingStack -> 8
        SStarting -> 9
        SStopped -> 10
        SStopping -> 11
        STraining -> 12
        SUploading -> 13
        SecondaryStatus' name -> (error . showText) $ "Unknown SecondaryStatus: " <> original name

-- | Represents the bounds of /known/ $SecondaryStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SecondaryStatus where
    minBound = SCompleted
    maxBound = SUploading

instance Hashable     SecondaryStatus
instance NFData       SecondaryStatus
instance ToByteString SecondaryStatus
instance ToQuery      SecondaryStatus
instance ToHeader     SecondaryStatus

instance FromJSON SecondaryStatus where
    parseJSON = parseJSONText "SecondaryStatus"
