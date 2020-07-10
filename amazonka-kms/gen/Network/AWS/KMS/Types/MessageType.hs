{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.MessageType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KMS.Types.MessageType (
  MessageType (
    ..
    , Digest
    , Raw
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MessageType = MessageType' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Digest :: MessageType
pattern Digest = MessageType' "DIGEST"

pattern Raw :: MessageType
pattern Raw = MessageType' "RAW"

{-# COMPLETE
  Digest,
  Raw,
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
        0 -> Digest
        1 -> Raw
        _ -> (error . showText) $ "Unknown index for MessageType: " <> toText i
    fromEnum x = case x of
        Digest -> 0
        Raw -> 1
        MessageType' name -> (error . showText) $ "Unknown MessageType: " <> original name

-- | Represents the bounds of /known/ $MessageType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MessageType where
    minBound = Digest
    maxBound = Raw

instance Hashable     MessageType
instance NFData       MessageType
instance ToByteString MessageType
instance ToQuery      MessageType
instance ToHeader     MessageType

instance ToJSON MessageType where
    toJSON = toJSONText
