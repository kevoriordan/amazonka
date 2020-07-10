{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.MessageFormatType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexRuntime.Types.MessageFormatType (
  MessageFormatType (
    ..
    , Composite
    , CustomPayload
    , PlainText
    , Ssml
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MessageFormatType = MessageFormatType' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern Composite :: MessageFormatType
pattern Composite = MessageFormatType' "Composite"

pattern CustomPayload :: MessageFormatType
pattern CustomPayload = MessageFormatType' "CustomPayload"

pattern PlainText :: MessageFormatType
pattern PlainText = MessageFormatType' "PlainText"

pattern Ssml :: MessageFormatType
pattern Ssml = MessageFormatType' "SSML"

{-# COMPLETE
  Composite,
  CustomPayload,
  PlainText,
  Ssml,
  MessageFormatType' #-}

instance FromText MessageFormatType where
    parser = (MessageFormatType' . mk) <$> takeText

instance ToText MessageFormatType where
    toText (MessageFormatType' ci) = original ci

-- | Represents an enum of /known/ $MessageFormatType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MessageFormatType where
    toEnum i = case i of
        0 -> Composite
        1 -> CustomPayload
        2 -> PlainText
        3 -> Ssml
        _ -> (error . showText) $ "Unknown index for MessageFormatType: " <> toText i
    fromEnum x = case x of
        Composite -> 0
        CustomPayload -> 1
        PlainText -> 2
        Ssml -> 3
        MessageFormatType' name -> (error . showText) $ "Unknown MessageFormatType: " <> original name

-- | Represents the bounds of /known/ $MessageFormatType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MessageFormatType where
    minBound = Composite
    maxBound = Ssml

instance Hashable     MessageFormatType
instance NFData       MessageFormatType
instance ToByteString MessageFormatType
instance ToQuery      MessageFormatType
instance ToHeader     MessageFormatType

instance ToJSON MessageFormatType where
    toJSON = toJSONText

instance FromJSON MessageFormatType where
    parseJSON = parseJSONText "MessageFormatType"
