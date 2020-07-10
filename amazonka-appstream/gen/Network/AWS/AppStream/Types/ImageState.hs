{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ImageState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.ImageState (
  ImageState (
    ..
    , ISAvailable
    , ISCopying
    , ISDeleting
    , ISFailed
    , ISPending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ImageState = ImageState' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern ISAvailable :: ImageState
pattern ISAvailable = ImageState' "AVAILABLE"

pattern ISCopying :: ImageState
pattern ISCopying = ImageState' "COPYING"

pattern ISDeleting :: ImageState
pattern ISDeleting = ImageState' "DELETING"

pattern ISFailed :: ImageState
pattern ISFailed = ImageState' "FAILED"

pattern ISPending :: ImageState
pattern ISPending = ImageState' "PENDING"

{-# COMPLETE
  ISAvailable,
  ISCopying,
  ISDeleting,
  ISFailed,
  ISPending,
  ImageState' #-}

instance FromText ImageState where
    parser = (ImageState' . mk) <$> takeText

instance ToText ImageState where
    toText (ImageState' ci) = original ci

-- | Represents an enum of /known/ $ImageState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ImageState where
    toEnum i = case i of
        0 -> ISAvailable
        1 -> ISCopying
        2 -> ISDeleting
        3 -> ISFailed
        4 -> ISPending
        _ -> (error . showText) $ "Unknown index for ImageState: " <> toText i
    fromEnum x = case x of
        ISAvailable -> 0
        ISCopying -> 1
        ISDeleting -> 2
        ISFailed -> 3
        ISPending -> 4
        ImageState' name -> (error . showText) $ "Unknown ImageState: " <> original name

-- | Represents the bounds of /known/ $ImageState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ImageState where
    minBound = ISAvailable
    maxBound = ISPending

instance Hashable     ImageState
instance NFData       ImageState
instance ToByteString ImageState
instance ToQuery      ImageState
instance ToHeader     ImageState

instance FromJSON ImageState where
    parseJSON = parseJSONText "ImageState"
