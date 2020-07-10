{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.Channel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.Channel (
  Channel (
    ..
    , Chat
    , Voice
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Channel = Channel' (CI Text)
                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                           Generic)

pattern Chat :: Channel
pattern Chat = Channel' "CHAT"

pattern Voice :: Channel
pattern Voice = Channel' "VOICE"

{-# COMPLETE
  Chat,
  Voice,
  Channel' #-}

instance FromText Channel where
    parser = (Channel' . mk) <$> takeText

instance ToText Channel where
    toText (Channel' ci) = original ci

-- | Represents an enum of /known/ $Channel.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Channel where
    toEnum i = case i of
        0 -> Chat
        1 -> Voice
        _ -> (error . showText) $ "Unknown index for Channel: " <> toText i
    fromEnum x = case x of
        Chat -> 0
        Voice -> 1
        Channel' name -> (error . showText) $ "Unknown Channel: " <> original name

-- | Represents the bounds of /known/ $Channel.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Channel where
    minBound = Chat
    maxBound = Voice

instance Hashable     Channel
instance NFData       Channel
instance ToByteString Channel
instance ToQuery      Channel
instance ToHeader     Channel

instance ToJSON Channel where
    toJSON = toJSONText

instance FromJSON Channel where
    parseJSON = parseJSONText "Channel"
