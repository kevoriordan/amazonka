{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContactProtocol
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.ContactProtocol (
  ContactProtocol (
    ..
    , Email
    , Sms
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ContactProtocol = ContactProtocol' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Email :: ContactProtocol
pattern Email = ContactProtocol' "Email"

pattern Sms :: ContactProtocol
pattern Sms = ContactProtocol' "SMS"

{-# COMPLETE
  Email,
  Sms,
  ContactProtocol' #-}

instance FromText ContactProtocol where
    parser = (ContactProtocol' . mk) <$> takeText

instance ToText ContactProtocol where
    toText (ContactProtocol' ci) = original ci

-- | Represents an enum of /known/ $ContactProtocol.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ContactProtocol where
    toEnum i = case i of
        0 -> Email
        1 -> Sms
        _ -> (error . showText) $ "Unknown index for ContactProtocol: " <> toText i
    fromEnum x = case x of
        Email -> 0
        Sms -> 1
        ContactProtocol' name -> (error . showText) $ "Unknown ContactProtocol: " <> original name

-- | Represents the bounds of /known/ $ContactProtocol.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ContactProtocol where
    minBound = Email
    maxBound = Sms

instance Hashable     ContactProtocol
instance NFData       ContactProtocol
instance ToByteString ContactProtocol
instance ToQuery      ContactProtocol
instance ToHeader     ContactProtocol

instance ToJSON ContactProtocol where
    toJSON = toJSONText

instance FromJSON ContactProtocol where
    parseJSON = parseJSONText "ContactProtocol"
