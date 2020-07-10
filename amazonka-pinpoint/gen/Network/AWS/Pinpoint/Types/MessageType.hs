{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.MessageType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.MessageType (
  MessageType (
    ..
    , Promotional
    , Transactional
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MessageType = MessageType' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Promotional :: MessageType
pattern Promotional = MessageType' "PROMOTIONAL"

pattern Transactional :: MessageType
pattern Transactional = MessageType' "TRANSACTIONAL"

{-# COMPLETE
  Promotional,
  Transactional,
  MessageType' #-}

instance FromText MessageType where
    parser = (MessageType' . mk) <$> takeText

instance ToText MessageType where
    toText (MessageType' ci) = original ci

-- | Represents an enum of /known/ $MessageType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MessageType where
    toEnum i = case i of
        0 -> Promotional
        1 -> Transactional
        _ -> (error . showText) $ "Unknown index for MessageType: " <> toText i
    fromEnum x = case x of
        Promotional -> 0
        Transactional -> 1
        MessageType' name -> (error . showText) $ "Unknown MessageType: " <> original name

-- | Represents the bounds of /known/ $MessageType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MessageType where
    minBound = Promotional
    maxBound = Transactional

instance Hashable     MessageType
instance NFData       MessageType
instance ToByteString MessageType
instance ToQuery      MessageType
instance ToHeader     MessageType

instance ToJSON MessageType where
    toJSON = toJSONText

instance FromJSON MessageType where
    parseJSON = parseJSONText "MessageType"
