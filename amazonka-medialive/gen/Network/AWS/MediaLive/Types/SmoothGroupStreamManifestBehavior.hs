{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.SmoothGroupStreamManifestBehavior
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.SmoothGroupStreamManifestBehavior (
  SmoothGroupStreamManifestBehavior (
    ..
    , DoNotSend
    , Send
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Smooth Group Stream Manifest Behavior
data SmoothGroupStreamManifestBehavior = SmoothGroupStreamManifestBehavior' (CI
                                                                               Text)
                                           deriving (Eq, Ord, Read, Show, Data,
                                                     Typeable, Generic)

pattern DoNotSend :: SmoothGroupStreamManifestBehavior
pattern DoNotSend = SmoothGroupStreamManifestBehavior' "DO_NOT_SEND"

pattern Send :: SmoothGroupStreamManifestBehavior
pattern Send = SmoothGroupStreamManifestBehavior' "SEND"

{-# COMPLETE
  DoNotSend,
  Send,
  SmoothGroupStreamManifestBehavior' #-}

instance FromText SmoothGroupStreamManifestBehavior where
    parser = (SmoothGroupStreamManifestBehavior' . mk) <$> takeText

instance ToText SmoothGroupStreamManifestBehavior where
    toText (SmoothGroupStreamManifestBehavior' ci) = original ci

-- | Represents an enum of /known/ $SmoothGroupStreamManifestBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SmoothGroupStreamManifestBehavior where
    toEnum i = case i of
        0 -> DoNotSend
        1 -> Send
        _ -> (error . showText) $ "Unknown index for SmoothGroupStreamManifestBehavior: " <> toText i
    fromEnum x = case x of
        DoNotSend -> 0
        Send -> 1
        SmoothGroupStreamManifestBehavior' name -> (error . showText) $ "Unknown SmoothGroupStreamManifestBehavior: " <> original name

-- | Represents the bounds of /known/ $SmoothGroupStreamManifestBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SmoothGroupStreamManifestBehavior where
    minBound = DoNotSend
    maxBound = Send

instance Hashable     SmoothGroupStreamManifestBehavior
instance NFData       SmoothGroupStreamManifestBehavior
instance ToByteString SmoothGroupStreamManifestBehavior
instance ToQuery      SmoothGroupStreamManifestBehavior
instance ToHeader     SmoothGroupStreamManifestBehavior

instance ToJSON SmoothGroupStreamManifestBehavior where
    toJSON = toJSONText

instance FromJSON SmoothGroupStreamManifestBehavior where
    parseJSON = parseJSONText "SmoothGroupStreamManifestBehavior"
