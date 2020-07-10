{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.MessageActionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.MessageActionType (
  MessageActionType (
    ..
    , Resend
    , Suppress
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MessageActionType = MessageActionType' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern Resend :: MessageActionType
pattern Resend = MessageActionType' "RESEND"

pattern Suppress :: MessageActionType
pattern Suppress = MessageActionType' "SUPPRESS"

{-# COMPLETE
  Resend,
  Suppress,
  MessageActionType' #-}

instance FromText MessageActionType where
    parser = (MessageActionType' . mk) <$> takeText

instance ToText MessageActionType where
    toText (MessageActionType' ci) = original ci

-- | Represents an enum of /known/ $MessageActionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MessageActionType where
    toEnum i = case i of
        0 -> Resend
        1 -> Suppress
        _ -> (error . showText) $ "Unknown index for MessageActionType: " <> toText i
    fromEnum x = case x of
        Resend -> 0
        Suppress -> 1
        MessageActionType' name -> (error . showText) $ "Unknown MessageActionType: " <> original name

-- | Represents the bounds of /known/ $MessageActionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MessageActionType where
    minBound = Resend
    maxBound = Suppress

instance Hashable     MessageActionType
instance NFData       MessageActionType
instance ToByteString MessageActionType
instance ToQuery      MessageActionType
instance ToHeader     MessageActionType

instance ToJSON MessageActionType where
    toJSON = toJSONText
