{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M3u8Scte35Behavior
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.M3u8Scte35Behavior (
  M3u8Scte35Behavior (
    ..
    , MSBNoPassthrough
    , MSBPassthrough
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | M3u8 Scte35 Behavior
data M3u8Scte35Behavior = M3u8Scte35Behavior' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern MSBNoPassthrough :: M3u8Scte35Behavior
pattern MSBNoPassthrough = M3u8Scte35Behavior' "NO_PASSTHROUGH"

pattern MSBPassthrough :: M3u8Scte35Behavior
pattern MSBPassthrough = M3u8Scte35Behavior' "PASSTHROUGH"

{-# COMPLETE
  MSBNoPassthrough,
  MSBPassthrough,
  M3u8Scte35Behavior' #-}

instance FromText M3u8Scte35Behavior where
    parser = (M3u8Scte35Behavior' . mk) <$> takeText

instance ToText M3u8Scte35Behavior where
    toText (M3u8Scte35Behavior' ci) = original ci

-- | Represents an enum of /known/ $M3u8Scte35Behavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum M3u8Scte35Behavior where
    toEnum i = case i of
        0 -> MSBNoPassthrough
        1 -> MSBPassthrough
        _ -> (error . showText) $ "Unknown index for M3u8Scte35Behavior: " <> toText i
    fromEnum x = case x of
        MSBNoPassthrough -> 0
        MSBPassthrough -> 1
        M3u8Scte35Behavior' name -> (error . showText) $ "Unknown M3u8Scte35Behavior: " <> original name

-- | Represents the bounds of /known/ $M3u8Scte35Behavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded M3u8Scte35Behavior where
    minBound = MSBNoPassthrough
    maxBound = MSBPassthrough

instance Hashable     M3u8Scte35Behavior
instance NFData       M3u8Scte35Behavior
instance ToByteString M3u8Scte35Behavior
instance ToQuery      M3u8Scte35Behavior
instance ToHeader     M3u8Scte35Behavior

instance ToJSON M3u8Scte35Behavior where
    toJSON = toJSONText

instance FromJSON M3u8Scte35Behavior where
    parseJSON = parseJSONText "M3u8Scte35Behavior"
