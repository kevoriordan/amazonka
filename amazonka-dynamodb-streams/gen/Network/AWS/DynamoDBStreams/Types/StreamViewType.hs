{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types.StreamViewType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDBStreams.Types.StreamViewType (
  StreamViewType (
    ..
    , KeysOnly
    , NewAndOldImages
    , NewImage
    , OldImage
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StreamViewType = StreamViewType' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern KeysOnly :: StreamViewType
pattern KeysOnly = StreamViewType' "KEYS_ONLY"

pattern NewAndOldImages :: StreamViewType
pattern NewAndOldImages = StreamViewType' "NEW_AND_OLD_IMAGES"

pattern NewImage :: StreamViewType
pattern NewImage = StreamViewType' "NEW_IMAGE"

pattern OldImage :: StreamViewType
pattern OldImage = StreamViewType' "OLD_IMAGE"

{-# COMPLETE
  KeysOnly,
  NewAndOldImages,
  NewImage,
  OldImage,
  StreamViewType' #-}

instance FromText StreamViewType where
    parser = (StreamViewType' . mk) <$> takeText

instance ToText StreamViewType where
    toText (StreamViewType' ci) = original ci

-- | Represents an enum of /known/ $StreamViewType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StreamViewType where
    toEnum i = case i of
        0 -> KeysOnly
        1 -> NewAndOldImages
        2 -> NewImage
        3 -> OldImage
        _ -> (error . showText) $ "Unknown index for StreamViewType: " <> toText i
    fromEnum x = case x of
        KeysOnly -> 0
        NewAndOldImages -> 1
        NewImage -> 2
        OldImage -> 3
        StreamViewType' name -> (error . showText) $ "Unknown StreamViewType: " <> original name

-- | Represents the bounds of /known/ $StreamViewType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StreamViewType where
    minBound = KeysOnly
    maxBound = OldImage

instance Hashable     StreamViewType
instance NFData       StreamViewType
instance ToByteString StreamViewType
instance ToQuery      StreamViewType
instance ToHeader     StreamViewType

instance FromJSON StreamViewType where
    parseJSON = parseJSONText "StreamViewType"
