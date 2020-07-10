{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M3u8NielsenId3Behavior
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.M3u8NielsenId3Behavior (
  M3u8NielsenId3Behavior (
    ..
    , MNoPassthrough
    , MPassthrough
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | M3u8 Nielsen Id3 Behavior
data M3u8NielsenId3Behavior = M3u8NielsenId3Behavior' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern MNoPassthrough :: M3u8NielsenId3Behavior
pattern MNoPassthrough = M3u8NielsenId3Behavior' "NO_PASSTHROUGH"

pattern MPassthrough :: M3u8NielsenId3Behavior
pattern MPassthrough = M3u8NielsenId3Behavior' "PASSTHROUGH"

{-# COMPLETE
  MNoPassthrough,
  MPassthrough,
  M3u8NielsenId3Behavior' #-}

instance FromText M3u8NielsenId3Behavior where
    parser = (M3u8NielsenId3Behavior' . mk) <$> takeText

instance ToText M3u8NielsenId3Behavior where
    toText (M3u8NielsenId3Behavior' ci) = original ci

-- | Represents an enum of /known/ $M3u8NielsenId3Behavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum M3u8NielsenId3Behavior where
    toEnum i = case i of
        0 -> MNoPassthrough
        1 -> MPassthrough
        _ -> (error . showText) $ "Unknown index for M3u8NielsenId3Behavior: " <> toText i
    fromEnum x = case x of
        MNoPassthrough -> 0
        MPassthrough -> 1
        M3u8NielsenId3Behavior' name -> (error . showText) $ "Unknown M3u8NielsenId3Behavior: " <> original name

-- | Represents the bounds of /known/ $M3u8NielsenId3Behavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded M3u8NielsenId3Behavior where
    minBound = MNoPassthrough
    maxBound = MPassthrough

instance Hashable     M3u8NielsenId3Behavior
instance NFData       M3u8NielsenId3Behavior
instance ToByteString M3u8NielsenId3Behavior
instance ToQuery      M3u8NielsenId3Behavior
instance ToHeader     M3u8NielsenId3Behavior

instance ToJSON M3u8NielsenId3Behavior where
    toJSON = toJSONText

instance FromJSON M3u8NielsenId3Behavior where
    parseJSON = parseJSONText "M3u8NielsenId3Behavior"
