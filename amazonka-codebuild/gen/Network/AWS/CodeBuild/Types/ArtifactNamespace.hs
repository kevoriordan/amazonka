{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ArtifactNamespace
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.ArtifactNamespace (
  ArtifactNamespace (
    ..
    , ANBuildId
    , ANNone
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ArtifactNamespace = ArtifactNamespace' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern ANBuildId :: ArtifactNamespace
pattern ANBuildId = ArtifactNamespace' "BUILD_ID"

pattern ANNone :: ArtifactNamespace
pattern ANNone = ArtifactNamespace' "NONE"

{-# COMPLETE
  ANBuildId,
  ANNone,
  ArtifactNamespace' #-}

instance FromText ArtifactNamespace where
    parser = (ArtifactNamespace' . mk) <$> takeText

instance ToText ArtifactNamespace where
    toText (ArtifactNamespace' ci) = original ci

-- | Represents an enum of /known/ $ArtifactNamespace.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ArtifactNamespace where
    toEnum i = case i of
        0 -> ANBuildId
        1 -> ANNone
        _ -> (error . showText) $ "Unknown index for ArtifactNamespace: " <> toText i
    fromEnum x = case x of
        ANBuildId -> 0
        ANNone -> 1
        ArtifactNamespace' name -> (error . showText) $ "Unknown ArtifactNamespace: " <> original name

-- | Represents the bounds of /known/ $ArtifactNamespace.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ArtifactNamespace where
    minBound = ANBuildId
    maxBound = ANNone

instance Hashable     ArtifactNamespace
instance NFData       ArtifactNamespace
instance ToByteString ArtifactNamespace
instance ToQuery      ArtifactNamespace
instance ToHeader     ArtifactNamespace

instance ToJSON ArtifactNamespace where
    toJSON = toJSONText

instance FromJSON ArtifactNamespace where
    parseJSON = parseJSONText "ArtifactNamespace"
