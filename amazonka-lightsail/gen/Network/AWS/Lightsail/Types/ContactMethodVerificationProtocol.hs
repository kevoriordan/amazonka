{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContactMethodVerificationProtocol
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.ContactMethodVerificationProtocol (
  ContactMethodVerificationProtocol (
    ..
    , CMVPEmail
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ContactMethodVerificationProtocol = ContactMethodVerificationProtocol' (CI
                                                                               Text)
                                           deriving (Eq, Ord, Read, Show, Data,
                                                     Typeable, Generic)

pattern CMVPEmail :: ContactMethodVerificationProtocol
pattern CMVPEmail = ContactMethodVerificationProtocol' "Email"

{-# COMPLETE
  CMVPEmail,
  ContactMethodVerificationProtocol' #-}

instance FromText ContactMethodVerificationProtocol where
    parser = (ContactMethodVerificationProtocol' . mk) <$> takeText

instance ToText ContactMethodVerificationProtocol where
    toText (ContactMethodVerificationProtocol' ci) = original ci

-- | Represents an enum of /known/ $ContactMethodVerificationProtocol.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ContactMethodVerificationProtocol where
    toEnum i = case i of
        0 -> CMVPEmail
        _ -> (error . showText) $ "Unknown index for ContactMethodVerificationProtocol: " <> toText i
    fromEnum x = case x of
        CMVPEmail -> 0
        ContactMethodVerificationProtocol' name -> (error . showText) $ "Unknown ContactMethodVerificationProtocol: " <> original name

-- | Represents the bounds of /known/ $ContactMethodVerificationProtocol.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ContactMethodVerificationProtocol where
    minBound = CMVPEmail
    maxBound = CMVPEmail

instance Hashable     ContactMethodVerificationProtocol
instance NFData       ContactMethodVerificationProtocol
instance ToByteString ContactMethodVerificationProtocol
instance ToQuery      ContactMethodVerificationProtocol
instance ToHeader     ContactMethodVerificationProtocol

instance ToJSON ContactMethodVerificationProtocol where
    toJSON = toJSONText
