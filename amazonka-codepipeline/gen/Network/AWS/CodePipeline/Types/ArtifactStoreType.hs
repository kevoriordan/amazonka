{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ArtifactStoreType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.ArtifactStoreType (
  ArtifactStoreType (
    ..
    , S3
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ArtifactStoreType = ArtifactStoreType' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern S3 :: ArtifactStoreType
pattern S3 = ArtifactStoreType' "S3"

{-# COMPLETE
  S3,
  ArtifactStoreType' #-}

instance FromText ArtifactStoreType where
    parser = (ArtifactStoreType' . mk) <$> takeText

instance ToText ArtifactStoreType where
    toText (ArtifactStoreType' ci) = original ci

-- | Represents an enum of /known/ $ArtifactStoreType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ArtifactStoreType where
    toEnum i = case i of
        0 -> S3
        _ -> (error . showText) $ "Unknown index for ArtifactStoreType: " <> toText i
    fromEnum x = case x of
        S3 -> 0
        ArtifactStoreType' name -> (error . showText) $ "Unknown ArtifactStoreType: " <> original name

-- | Represents the bounds of /known/ $ArtifactStoreType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ArtifactStoreType where
    minBound = S3
    maxBound = S3

instance Hashable     ArtifactStoreType
instance NFData       ArtifactStoreType
instance ToByteString ArtifactStoreType
instance ToQuery      ArtifactStoreType
instance ToHeader     ArtifactStoreType

instance ToJSON ArtifactStoreType where
    toJSON = toJSONText

instance FromJSON ArtifactStoreType where
    parseJSON = parseJSONText "ArtifactStoreType"
