{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.EventFilterType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.EventFilterType (
  EventFilterType (
    ..
    , PasswordChange
    , SignIn
    , SignUp
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EventFilterType = EventFilterType' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern PasswordChange :: EventFilterType
pattern PasswordChange = EventFilterType' "PASSWORD_CHANGE"

pattern SignIn :: EventFilterType
pattern SignIn = EventFilterType' "SIGN_IN"

pattern SignUp :: EventFilterType
pattern SignUp = EventFilterType' "SIGN_UP"

{-# COMPLETE
  PasswordChange,
  SignIn,
  SignUp,
  EventFilterType' #-}

instance FromText EventFilterType where
    parser = (EventFilterType' . mk) <$> takeText

instance ToText EventFilterType where
    toText (EventFilterType' ci) = original ci

-- | Represents an enum of /known/ $EventFilterType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EventFilterType where
    toEnum i = case i of
        0 -> PasswordChange
        1 -> SignIn
        2 -> SignUp
        _ -> (error . showText) $ "Unknown index for EventFilterType: " <> toText i
    fromEnum x = case x of
        PasswordChange -> 0
        SignIn -> 1
        SignUp -> 2
        EventFilterType' name -> (error . showText) $ "Unknown EventFilterType: " <> original name

-- | Represents the bounds of /known/ $EventFilterType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EventFilterType where
    minBound = PasswordChange
    maxBound = SignUp

instance Hashable     EventFilterType
instance NFData       EventFilterType
instance ToByteString EventFilterType
instance ToQuery      EventFilterType
instance ToHeader     EventFilterType

instance ToJSON EventFilterType where
    toJSON = toJSONText

instance FromJSON EventFilterType where
    parseJSON = parseJSONText "EventFilterType"
