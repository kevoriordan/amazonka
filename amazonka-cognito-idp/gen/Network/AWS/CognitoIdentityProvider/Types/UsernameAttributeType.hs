{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UsernameAttributeType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.UsernameAttributeType (
  UsernameAttributeType (
    ..
    , UATEmail
    , UATPhoneNumber
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data UsernameAttributeType = UsernameAttributeType' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern UATEmail :: UsernameAttributeType
pattern UATEmail = UsernameAttributeType' "email"

pattern UATPhoneNumber :: UsernameAttributeType
pattern UATPhoneNumber = UsernameAttributeType' "phone_number"

{-# COMPLETE
  UATEmail,
  UATPhoneNumber,
  UsernameAttributeType' #-}

instance FromText UsernameAttributeType where
    parser = (UsernameAttributeType' . mk) <$> takeText

instance ToText UsernameAttributeType where
    toText (UsernameAttributeType' ci) = original ci

-- | Represents an enum of /known/ $UsernameAttributeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum UsernameAttributeType where
    toEnum i = case i of
        0 -> UATEmail
        1 -> UATPhoneNumber
        _ -> (error . showText) $ "Unknown index for UsernameAttributeType: " <> toText i
    fromEnum x = case x of
        UATEmail -> 0
        UATPhoneNumber -> 1
        UsernameAttributeType' name -> (error . showText) $ "Unknown UsernameAttributeType: " <> original name

-- | Represents the bounds of /known/ $UsernameAttributeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded UsernameAttributeType where
    minBound = UATEmail
    maxBound = UATPhoneNumber

instance Hashable     UsernameAttributeType
instance NFData       UsernameAttributeType
instance ToByteString UsernameAttributeType
instance ToQuery      UsernameAttributeType
instance ToHeader     UsernameAttributeType

instance ToJSON UsernameAttributeType where
    toJSON = toJSONText

instance FromJSON UsernameAttributeType where
    parseJSON = parseJSONText "UsernameAttributeType"
