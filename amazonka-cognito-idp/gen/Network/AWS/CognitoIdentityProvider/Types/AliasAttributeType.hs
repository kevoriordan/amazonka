{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AliasAttributeType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.AliasAttributeType (
  AliasAttributeType (
    ..
    , AATEmail
    , AATPhoneNumber
    , AATPreferredUsername
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AliasAttributeType = AliasAttributeType' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern AATEmail :: AliasAttributeType
pattern AATEmail = AliasAttributeType' "email"

pattern AATPhoneNumber :: AliasAttributeType
pattern AATPhoneNumber = AliasAttributeType' "phone_number"

pattern AATPreferredUsername :: AliasAttributeType
pattern AATPreferredUsername = AliasAttributeType' "preferred_username"

{-# COMPLETE
  AATEmail,
  AATPhoneNumber,
  AATPreferredUsername,
  AliasAttributeType' #-}

instance FromText AliasAttributeType where
    parser = (AliasAttributeType' . mk) <$> takeText

instance ToText AliasAttributeType where
    toText (AliasAttributeType' ci) = original ci

-- | Represents an enum of /known/ $AliasAttributeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AliasAttributeType where
    toEnum i = case i of
        0 -> AATEmail
        1 -> AATPhoneNumber
        2 -> AATPreferredUsername
        _ -> (error . showText) $ "Unknown index for AliasAttributeType: " <> toText i
    fromEnum x = case x of
        AATEmail -> 0
        AATPhoneNumber -> 1
        AATPreferredUsername -> 2
        AliasAttributeType' name -> (error . showText) $ "Unknown AliasAttributeType: " <> original name

-- | Represents the bounds of /known/ $AliasAttributeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AliasAttributeType where
    minBound = AATEmail
    maxBound = AATPreferredUsername

instance Hashable     AliasAttributeType
instance NFData       AliasAttributeType
instance ToByteString AliasAttributeType
instance ToQuery      AliasAttributeType
instance ToHeader     AliasAttributeType

instance ToJSON AliasAttributeType where
    toJSON = toJSONText

instance FromJSON AliasAttributeType where
    parseJSON = parseJSONText "AliasAttributeType"
