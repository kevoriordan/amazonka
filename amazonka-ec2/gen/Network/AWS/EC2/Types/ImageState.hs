{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ImageState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ImageState (
  ImageState (
    ..
    , ISAvailable
    , ISDeregistered
    , ISError'
    , ISFailed
    , ISInvalid
    , ISPending
    , ISTransient
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ImageState = ImageState' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern ISAvailable :: ImageState
pattern ISAvailable = ImageState' "available"

pattern ISDeregistered :: ImageState
pattern ISDeregistered = ImageState' "deregistered"

pattern ISError' :: ImageState
pattern ISError' = ImageState' "error"

pattern ISFailed :: ImageState
pattern ISFailed = ImageState' "failed"

pattern ISInvalid :: ImageState
pattern ISInvalid = ImageState' "invalid"

pattern ISPending :: ImageState
pattern ISPending = ImageState' "pending"

pattern ISTransient :: ImageState
pattern ISTransient = ImageState' "transient"

{-# COMPLETE
  ISAvailable,
  ISDeregistered,
  ISError',
  ISFailed,
  ISInvalid,
  ISPending,
  ISTransient,
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
        1 -> ISDeregistered
        2 -> ISError'
        3 -> ISFailed
        4 -> ISInvalid
        5 -> ISPending
        6 -> ISTransient
        _ -> (error . showText) $ "Unknown index for ImageState: " <> toText i
    fromEnum x = case x of
        ISAvailable -> 0
        ISDeregistered -> 1
        ISError' -> 2
        ISFailed -> 3
        ISInvalid -> 4
        ISPending -> 5
        ISTransient -> 6
        ImageState' name -> (error . showText) $ "Unknown ImageState: " <> original name

-- | Represents the bounds of /known/ $ImageState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ImageState where
    minBound = ISAvailable
    maxBound = ISTransient

instance Hashable     ImageState
instance NFData       ImageState
instance ToByteString ImageState
instance ToQuery      ImageState
instance ToHeader     ImageState

instance FromXML ImageState where
    parseXML = parseXMLText "ImageState"
