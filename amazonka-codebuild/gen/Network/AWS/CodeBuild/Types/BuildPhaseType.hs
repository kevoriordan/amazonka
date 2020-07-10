{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BuildPhaseType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.BuildPhaseType (
  BuildPhaseType (
    ..
    , Build
    , Completed
    , DownloadSource
    , Finalizing
    , Install
    , PostBuild
    , PreBuild
    , Provisioning
    , Queued
    , Submitted
    , UploadArtifacts
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data BuildPhaseType = BuildPhaseType' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern Build :: BuildPhaseType
pattern Build = BuildPhaseType' "BUILD"

pattern Completed :: BuildPhaseType
pattern Completed = BuildPhaseType' "COMPLETED"

pattern DownloadSource :: BuildPhaseType
pattern DownloadSource = BuildPhaseType' "DOWNLOAD_SOURCE"

pattern Finalizing :: BuildPhaseType
pattern Finalizing = BuildPhaseType' "FINALIZING"

pattern Install :: BuildPhaseType
pattern Install = BuildPhaseType' "INSTALL"

pattern PostBuild :: BuildPhaseType
pattern PostBuild = BuildPhaseType' "POST_BUILD"

pattern PreBuild :: BuildPhaseType
pattern PreBuild = BuildPhaseType' "PRE_BUILD"

pattern Provisioning :: BuildPhaseType
pattern Provisioning = BuildPhaseType' "PROVISIONING"

pattern Queued :: BuildPhaseType
pattern Queued = BuildPhaseType' "QUEUED"

pattern Submitted :: BuildPhaseType
pattern Submitted = BuildPhaseType' "SUBMITTED"

pattern UploadArtifacts :: BuildPhaseType
pattern UploadArtifacts = BuildPhaseType' "UPLOAD_ARTIFACTS"

{-# COMPLETE
  Build,
  Completed,
  DownloadSource,
  Finalizing,
  Install,
  PostBuild,
  PreBuild,
  Provisioning,
  Queued,
  Submitted,
  UploadArtifacts,
  BuildPhaseType' #-}

instance FromText BuildPhaseType where
    parser = (BuildPhaseType' . mk) <$> takeText

instance ToText BuildPhaseType where
    toText (BuildPhaseType' ci) = original ci

-- | Represents an enum of /known/ $BuildPhaseType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BuildPhaseType where
    toEnum i = case i of
        0 -> Build
        1 -> Completed
        2 -> DownloadSource
        3 -> Finalizing
        4 -> Install
        5 -> PostBuild
        6 -> PreBuild
        7 -> Provisioning
        8 -> Queued
        9 -> Submitted
        10 -> UploadArtifacts
        _ -> (error . showText) $ "Unknown index for BuildPhaseType: " <> toText i
    fromEnum x = case x of
        Build -> 0
        Completed -> 1
        DownloadSource -> 2
        Finalizing -> 3
        Install -> 4
        PostBuild -> 5
        PreBuild -> 6
        Provisioning -> 7
        Queued -> 8
        Submitted -> 9
        UploadArtifacts -> 10
        BuildPhaseType' name -> (error . showText) $ "Unknown BuildPhaseType: " <> original name

-- | Represents the bounds of /known/ $BuildPhaseType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BuildPhaseType where
    minBound = Build
    maxBound = UploadArtifacts

instance Hashable     BuildPhaseType
instance NFData       BuildPhaseType
instance ToByteString BuildPhaseType
instance ToQuery      BuildPhaseType
instance ToHeader     BuildPhaseType

instance FromJSON BuildPhaseType where
    parseJSON = parseJSONText "BuildPhaseType"
