{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ImageFailureCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECR.Types.ImageFailureCode (
  ImageFailureCode (
    ..
    , ImageNotFound
    , ImageReferencedByManifestList
    , ImageTagDoesNotMatchDigest
    , InvalidImageDigest
    , InvalidImageTag
    , MissingDigestAndTag
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ImageFailureCode = ImageFailureCode' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern ImageNotFound :: ImageFailureCode
pattern ImageNotFound = ImageFailureCode' "ImageNotFound"

pattern ImageReferencedByManifestList :: ImageFailureCode
pattern ImageReferencedByManifestList = ImageFailureCode' "ImageReferencedByManifestList"

pattern ImageTagDoesNotMatchDigest :: ImageFailureCode
pattern ImageTagDoesNotMatchDigest = ImageFailureCode' "ImageTagDoesNotMatchDigest"

pattern InvalidImageDigest :: ImageFailureCode
pattern InvalidImageDigest = ImageFailureCode' "InvalidImageDigest"

pattern InvalidImageTag :: ImageFailureCode
pattern InvalidImageTag = ImageFailureCode' "InvalidImageTag"

pattern MissingDigestAndTag :: ImageFailureCode
pattern MissingDigestAndTag = ImageFailureCode' "MissingDigestAndTag"

{-# COMPLETE
  ImageNotFound,
  ImageReferencedByManifestList,
  ImageTagDoesNotMatchDigest,
  InvalidImageDigest,
  InvalidImageTag,
  MissingDigestAndTag,
  ImageFailureCode' #-}

instance FromText ImageFailureCode where
    parser = (ImageFailureCode' . mk) <$> takeText

instance ToText ImageFailureCode where
    toText (ImageFailureCode' ci) = original ci

-- | Represents an enum of /known/ $ImageFailureCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ImageFailureCode where
    toEnum i = case i of
        0 -> ImageNotFound
        1 -> ImageReferencedByManifestList
        2 -> ImageTagDoesNotMatchDigest
        3 -> InvalidImageDigest
        4 -> InvalidImageTag
        5 -> MissingDigestAndTag
        _ -> (error . showText) $ "Unknown index for ImageFailureCode: " <> toText i
    fromEnum x = case x of
        ImageNotFound -> 0
        ImageReferencedByManifestList -> 1
        ImageTagDoesNotMatchDigest -> 2
        InvalidImageDigest -> 3
        InvalidImageTag -> 4
        MissingDigestAndTag -> 5
        ImageFailureCode' name -> (error . showText) $ "Unknown ImageFailureCode: " <> original name

-- | Represents the bounds of /known/ $ImageFailureCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ImageFailureCode where
    minBound = ImageNotFound
    maxBound = MissingDigestAndTag

instance Hashable     ImageFailureCode
instance NFData       ImageFailureCode
instance ToByteString ImageFailureCode
instance ToQuery      ImageFailureCode
instance ToHeader     ImageFailureCode

instance FromJSON ImageFailureCode where
    parseJSON = parseJSONText "ImageFailureCode"
