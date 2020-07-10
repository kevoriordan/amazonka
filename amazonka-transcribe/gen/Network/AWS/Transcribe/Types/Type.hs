{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.Type
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Transcribe.Types.Type (
  Type (
    ..
    , Conversation
    , Dictation
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Type = Type' (CI Text)
              deriving (Eq, Ord, Read, Show, Data, Typeable,
                        Generic)

pattern Conversation :: Type
pattern Conversation = Type' "CONVERSATION"

pattern Dictation :: Type
pattern Dictation = Type' "DICTATION"

{-# COMPLETE
  Conversation,
  Dictation,
  Type' #-}

instance FromText Type where
    parser = (Type' . mk) <$> takeText

instance ToText Type where
    toText (Type' ci) = original ci

-- | Represents an enum of /known/ $Type.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Type where
    toEnum i = case i of
        0 -> Conversation
        1 -> Dictation
        _ -> (error . showText) $ "Unknown index for Type: " <> toText i
    fromEnum x = case x of
        Conversation -> 0
        Dictation -> 1
        Type' name -> (error . showText) $ "Unknown Type: " <> original name

-- | Represents the bounds of /known/ $Type.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Type where
    minBound = Conversation
    maxBound = Dictation

instance Hashable     Type
instance NFData       Type
instance ToByteString Type
instance ToQuery      Type
instance ToHeader     Type

instance ToJSON Type where
    toJSON = toJSONText

instance FromJSON Type where
    parseJSON = parseJSONText "Type"
