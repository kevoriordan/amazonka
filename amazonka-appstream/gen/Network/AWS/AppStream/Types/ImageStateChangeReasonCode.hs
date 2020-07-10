{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ImageStateChangeReasonCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.ImageStateChangeReasonCode (
  ImageStateChangeReasonCode (
    ..
    , ISCRCImageBuilderNotAvailable
    , ISCRCImageCopyFailure
    , ISCRCInternalError
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ImageStateChangeReasonCode = ImageStateChangeReasonCode' (CI
                                                                 Text)
                                    deriving (Eq, Ord, Read, Show, Data,
                                              Typeable, Generic)

pattern ISCRCImageBuilderNotAvailable :: ImageStateChangeReasonCode
pattern ISCRCImageBuilderNotAvailable = ImageStateChangeReasonCode' "IMAGE_BUILDER_NOT_AVAILABLE"

pattern ISCRCImageCopyFailure :: ImageStateChangeReasonCode
pattern ISCRCImageCopyFailure = ImageStateChangeReasonCode' "IMAGE_COPY_FAILURE"

pattern ISCRCInternalError :: ImageStateChangeReasonCode
pattern ISCRCInternalError = ImageStateChangeReasonCode' "INTERNAL_ERROR"

{-# COMPLETE
  ISCRCImageBuilderNotAvailable,
  ISCRCImageCopyFailure,
  ISCRCInternalError,
  ImageStateChangeReasonCode' #-}

instance FromText ImageStateChangeReasonCode where
    parser = (ImageStateChangeReasonCode' . mk) <$> takeText

instance ToText ImageStateChangeReasonCode where
    toText (ImageStateChangeReasonCode' ci) = original ci

-- | Represents an enum of /known/ $ImageStateChangeReasonCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ImageStateChangeReasonCode where
    toEnum i = case i of
        0 -> ISCRCImageBuilderNotAvailable
        1 -> ISCRCImageCopyFailure
        2 -> ISCRCInternalError
        _ -> (error . showText) $ "Unknown index for ImageStateChangeReasonCode: " <> toText i
    fromEnum x = case x of
        ISCRCImageBuilderNotAvailable -> 0
        ISCRCImageCopyFailure -> 1
        ISCRCInternalError -> 2
        ImageStateChangeReasonCode' name -> (error . showText) $ "Unknown ImageStateChangeReasonCode: " <> original name

-- | Represents the bounds of /known/ $ImageStateChangeReasonCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ImageStateChangeReasonCode where
    minBound = ISCRCImageBuilderNotAvailable
    maxBound = ISCRCInternalError

instance Hashable     ImageStateChangeReasonCode
instance NFData       ImageStateChangeReasonCode
instance ToByteString ImageStateChangeReasonCode
instance ToQuery      ImageStateChangeReasonCode
instance ToHeader     ImageStateChangeReasonCode

instance FromJSON ImageStateChangeReasonCode where
    parseJSON = parseJSONText "ImageStateChangeReasonCode"
