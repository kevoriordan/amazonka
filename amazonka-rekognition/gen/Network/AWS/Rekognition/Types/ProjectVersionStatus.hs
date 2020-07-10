{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ProjectVersionStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.ProjectVersionStatus (
  ProjectVersionStatus (
    ..
    , Deleting
    , Failed
    , Running
    , Starting
    , Stopped
    , Stopping
    , TrainingCompleted
    , TrainingFailed
    , TrainingInProgress
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ProjectVersionStatus = ProjectVersionStatus' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern Deleting :: ProjectVersionStatus
pattern Deleting = ProjectVersionStatus' "DELETING"

pattern Failed :: ProjectVersionStatus
pattern Failed = ProjectVersionStatus' "FAILED"

pattern Running :: ProjectVersionStatus
pattern Running = ProjectVersionStatus' "RUNNING"

pattern Starting :: ProjectVersionStatus
pattern Starting = ProjectVersionStatus' "STARTING"

pattern Stopped :: ProjectVersionStatus
pattern Stopped = ProjectVersionStatus' "STOPPED"

pattern Stopping :: ProjectVersionStatus
pattern Stopping = ProjectVersionStatus' "STOPPING"

pattern TrainingCompleted :: ProjectVersionStatus
pattern TrainingCompleted = ProjectVersionStatus' "TRAINING_COMPLETED"

pattern TrainingFailed :: ProjectVersionStatus
pattern TrainingFailed = ProjectVersionStatus' "TRAINING_FAILED"

pattern TrainingInProgress :: ProjectVersionStatus
pattern TrainingInProgress = ProjectVersionStatus' "TRAINING_IN_PROGRESS"

{-# COMPLETE
  Deleting,
  Failed,
  Running,
  Starting,
  Stopped,
  Stopping,
  TrainingCompleted,
  TrainingFailed,
  TrainingInProgress,
  ProjectVersionStatus' #-}

instance FromText ProjectVersionStatus where
    parser = (ProjectVersionStatus' . mk) <$> takeText

instance ToText ProjectVersionStatus where
    toText (ProjectVersionStatus' ci) = original ci

-- | Represents an enum of /known/ $ProjectVersionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProjectVersionStatus where
    toEnum i = case i of
        0 -> Deleting
        1 -> Failed
        2 -> Running
        3 -> Starting
        4 -> Stopped
        5 -> Stopping
        6 -> TrainingCompleted
        7 -> TrainingFailed
        8 -> TrainingInProgress
        _ -> (error . showText) $ "Unknown index for ProjectVersionStatus: " <> toText i
    fromEnum x = case x of
        Deleting -> 0
        Failed -> 1
        Running -> 2
        Starting -> 3
        Stopped -> 4
        Stopping -> 5
        TrainingCompleted -> 6
        TrainingFailed -> 7
        TrainingInProgress -> 8
        ProjectVersionStatus' name -> (error . showText) $ "Unknown ProjectVersionStatus: " <> original name

-- | Represents the bounds of /known/ $ProjectVersionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProjectVersionStatus where
    minBound = Deleting
    maxBound = TrainingInProgress

instance Hashable     ProjectVersionStatus
instance NFData       ProjectVersionStatus
instance ToByteString ProjectVersionStatus
instance ToQuery      ProjectVersionStatus
instance ToHeader     ProjectVersionStatus

instance FromJSON ProjectVersionStatus where
    parseJSON = parseJSONText "ProjectVersionStatus"
