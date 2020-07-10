{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.VerifiedAttributeType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.VerifiedAttributeType (
  VerifiedAttributeType (
    ..
    , Email
    , PhoneNumber
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data VerifiedAttributeType = VerifiedAttributeType' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern Email :: VerifiedAttributeType
pattern Email = VerifiedAttributeType' "email"

pattern PhoneNumber :: VerifiedAttributeType
pattern PhoneNumber = VerifiedAttributeType' "phone_number"

{-# COMPLETE
  Email,
  PhoneNumber,
  VerifiedAttributeType' #-}

instance FromText VerifiedAttributeType where
    parser = (VerifiedAttributeType' . mk) <$> takeText

instance ToText VerifiedAttributeType where
    toText (VerifiedAttributeType' ci) = original ci

-- | Represents an enum of /known/ $VerifiedAttributeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum VerifiedAttributeType where
    toEnum i = case i of
        0 -> Email
        1 -> PhoneNumber
        _ -> (error . showText) $ "Unknown index for VerifiedAttributeType: " <> toText i
    fromEnum x = case x of
        Email -> 0
        PhoneNumber -> 1
        VerifiedAttributeType' name -> (error . showText) $ "Unknown VerifiedAttributeType: " <> original name

-- | Represents the bounds of /known/ $VerifiedAttributeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded VerifiedAttributeType where
    minBound = Email
    maxBound = PhoneNumber

instance Hashable     VerifiedAttributeType
instance NFData       VerifiedAttributeType
instance ToByteString VerifiedAttributeType
instance ToQuery      VerifiedAttributeType
instance ToHeader     VerifiedAttributeType

instance ToJSON VerifiedAttributeType where
    toJSON = toJSONText

instance FromJSON VerifiedAttributeType where
    parseJSON = parseJSONText "VerifiedAttributeType"
