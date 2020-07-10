{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ArtifactLocationType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.ArtifactLocationType (
  ArtifactLocationType (
    ..
    , ALTS3
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ArtifactLocationType = ArtifactLocationType' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern ALTS3 :: ArtifactLocationType
pattern ALTS3 = ArtifactLocationType' "S3"

{-# COMPLETE
  ALTS3,
  ArtifactLocationType' #-}

instance FromText ArtifactLocationType where
    parser = (ArtifactLocationType' . mk) <$> takeText

instance ToText ArtifactLocationType where
    toText (ArtifactLocationType' ci) = original ci

-- | Represents an enum of /known/ $ArtifactLocationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ArtifactLocationType where
    toEnum i = case i of
        0 -> ALTS3
        _ -> (error . showText) $ "Unknown index for ArtifactLocationType: " <> toText i
    fromEnum x = case x of
        ALTS3 -> 0
        ArtifactLocationType' name -> (error . showText) $ "Unknown ArtifactLocationType: " <> original name

-- | Represents the bounds of /known/ $ArtifactLocationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ArtifactLocationType where
    minBound = ALTS3
    maxBound = ALTS3

instance Hashable     ArtifactLocationType
instance NFData       ArtifactLocationType
instance ToByteString ArtifactLocationType
instance ToQuery      ArtifactLocationType
instance ToHeader     ArtifactLocationType

instance FromJSON ArtifactLocationType where
    parseJSON = parseJSONText "ArtifactLocationType"
