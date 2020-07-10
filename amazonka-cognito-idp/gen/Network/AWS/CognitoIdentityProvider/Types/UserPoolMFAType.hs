{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserPoolMFAType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.UserPoolMFAType (
  UserPoolMFAType (
    ..
    , UPMTON
    , UPMTOff
    , UPMTOptional
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data UserPoolMFAType = UserPoolMFAType' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern UPMTON :: UserPoolMFAType
pattern UPMTON = UserPoolMFAType' "ON"

pattern UPMTOff :: UserPoolMFAType
pattern UPMTOff = UserPoolMFAType' "OFF"

pattern UPMTOptional :: UserPoolMFAType
pattern UPMTOptional = UserPoolMFAType' "OPTIONAL"

{-# COMPLETE
  UPMTON,
  UPMTOff,
  UPMTOptional,
  UserPoolMFAType' #-}

instance FromText UserPoolMFAType where
    parser = (UserPoolMFAType' . mk) <$> takeText

instance ToText UserPoolMFAType where
    toText (UserPoolMFAType' ci) = original ci

-- | Represents an enum of /known/ $UserPoolMFAType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum UserPoolMFAType where
    toEnum i = case i of
        0 -> UPMTON
        1 -> UPMTOff
        2 -> UPMTOptional
        _ -> (error . showText) $ "Unknown index for UserPoolMFAType: " <> toText i
    fromEnum x = case x of
        UPMTON -> 0
        UPMTOff -> 1
        UPMTOptional -> 2
        UserPoolMFAType' name -> (error . showText) $ "Unknown UserPoolMFAType: " <> original name

-- | Represents the bounds of /known/ $UserPoolMFAType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded UserPoolMFAType where
    minBound = UPMTON
    maxBound = UPMTOptional

instance Hashable     UserPoolMFAType
instance NFData       UserPoolMFAType
instance ToByteString UserPoolMFAType
instance ToQuery      UserPoolMFAType
instance ToHeader     UserPoolMFAType

instance ToJSON UserPoolMFAType where
    toJSON = toJSONText

instance FromJSON UserPoolMFAType where
    parseJSON = parseJSONText "UserPoolMFAType"
