{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.RecoveryOptionNameType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.RecoveryOptionNameType (
  RecoveryOptionNameType (
    ..
    , AdminOnly
    , VerifiedEmail
    , VerifiedPhoneNumber
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RecoveryOptionNameType = RecoveryOptionNameType' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern AdminOnly :: RecoveryOptionNameType
pattern AdminOnly = RecoveryOptionNameType' "admin_only"

pattern VerifiedEmail :: RecoveryOptionNameType
pattern VerifiedEmail = RecoveryOptionNameType' "verified_email"

pattern VerifiedPhoneNumber :: RecoveryOptionNameType
pattern VerifiedPhoneNumber = RecoveryOptionNameType' "verified_phone_number"

{-# COMPLETE
  AdminOnly,
  VerifiedEmail,
  VerifiedPhoneNumber,
  RecoveryOptionNameType' #-}

instance FromText RecoveryOptionNameType where
    parser = (RecoveryOptionNameType' . mk) <$> takeText

instance ToText RecoveryOptionNameType where
    toText (RecoveryOptionNameType' ci) = original ci

-- | Represents an enum of /known/ $RecoveryOptionNameType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RecoveryOptionNameType where
    toEnum i = case i of
        0 -> AdminOnly
        1 -> VerifiedEmail
        2 -> VerifiedPhoneNumber
        _ -> (error . showText) $ "Unknown index for RecoveryOptionNameType: " <> toText i
    fromEnum x = case x of
        AdminOnly -> 0
        VerifiedEmail -> 1
        VerifiedPhoneNumber -> 2
        RecoveryOptionNameType' name -> (error . showText) $ "Unknown RecoveryOptionNameType: " <> original name

-- | Represents the bounds of /known/ $RecoveryOptionNameType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RecoveryOptionNameType where
    minBound = AdminOnly
    maxBound = VerifiedPhoneNumber

instance Hashable     RecoveryOptionNameType
instance NFData       RecoveryOptionNameType
instance ToByteString RecoveryOptionNameType
instance ToQuery      RecoveryOptionNameType
instance ToHeader     RecoveryOptionNameType

instance ToJSON RecoveryOptionNameType where
    toJSON = toJSONText

instance FromJSON RecoveryOptionNameType where
    parseJSON = parseJSONText "RecoveryOptionNameType"
