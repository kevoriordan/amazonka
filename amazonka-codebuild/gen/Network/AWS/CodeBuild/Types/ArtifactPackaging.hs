{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ArtifactPackaging
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.ArtifactPackaging (
  ArtifactPackaging (
    ..
    , None
    , Zip
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ArtifactPackaging = ArtifactPackaging' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern None :: ArtifactPackaging
pattern None = ArtifactPackaging' "NONE"

pattern Zip :: ArtifactPackaging
pattern Zip = ArtifactPackaging' "ZIP"

{-# COMPLETE
  None,
  Zip,
  ArtifactPackaging' #-}

instance FromText ArtifactPackaging where
    parser = (ArtifactPackaging' . mk) <$> takeText

instance ToText ArtifactPackaging where
    toText (ArtifactPackaging' ci) = original ci

-- | Represents an enum of /known/ $ArtifactPackaging.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ArtifactPackaging where
    toEnum i = case i of
        0 -> None
        1 -> Zip
        _ -> (error . showText) $ "Unknown index for ArtifactPackaging: " <> toText i
    fromEnum x = case x of
        None -> 0
        Zip -> 1
        ArtifactPackaging' name -> (error . showText) $ "Unknown ArtifactPackaging: " <> original name

-- | Represents the bounds of /known/ $ArtifactPackaging.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ArtifactPackaging where
    minBound = None
    maxBound = Zip

instance Hashable     ArtifactPackaging
instance NFData       ArtifactPackaging
instance ToByteString ArtifactPackaging
instance ToQuery      ArtifactPackaging
instance ToHeader     ArtifactPackaging

instance ToJSON ArtifactPackaging where
    toJSON = toJSONText

instance FromJSON ArtifactPackaging where
    parseJSON = parseJSONText "ArtifactPackaging"
