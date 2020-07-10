{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M3u8NielsenId3
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.M3u8NielsenId3 (
  M3u8NielsenId3 (
    ..
    , M3uInsert
    , M3uNone
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | If INSERT, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
data M3u8NielsenId3 = M3u8NielsenId3' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern M3uInsert :: M3u8NielsenId3
pattern M3uInsert = M3u8NielsenId3' "INSERT"

pattern M3uNone :: M3u8NielsenId3
pattern M3uNone = M3u8NielsenId3' "NONE"

{-# COMPLETE
  M3uInsert,
  M3uNone,
  M3u8NielsenId3' #-}

instance FromText M3u8NielsenId3 where
    parser = (M3u8NielsenId3' . mk) <$> takeText

instance ToText M3u8NielsenId3 where
    toText (M3u8NielsenId3' ci) = original ci

-- | Represents an enum of /known/ $M3u8NielsenId3.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum M3u8NielsenId3 where
    toEnum i = case i of
        0 -> M3uInsert
        1 -> M3uNone
        _ -> (error . showText) $ "Unknown index for M3u8NielsenId3: " <> toText i
    fromEnum x = case x of
        M3uInsert -> 0
        M3uNone -> 1
        M3u8NielsenId3' name -> (error . showText) $ "Unknown M3u8NielsenId3: " <> original name

-- | Represents the bounds of /known/ $M3u8NielsenId3.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded M3u8NielsenId3 where
    minBound = M3uInsert
    maxBound = M3uNone

instance Hashable     M3u8NielsenId3
instance NFData       M3u8NielsenId3
instance ToByteString M3u8NielsenId3
instance ToQuery      M3u8NielsenId3
instance ToHeader     M3u8NielsenId3

instance ToJSON M3u8NielsenId3 where
    toJSON = toJSONText

instance FromJSON M3u8NielsenId3 where
    parseJSON = parseJSONText "M3u8NielsenId3"
