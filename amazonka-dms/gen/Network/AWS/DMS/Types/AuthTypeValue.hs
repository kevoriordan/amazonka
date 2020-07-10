{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.AuthTypeValue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.AuthTypeValue (
  AuthTypeValue (
    ..
    , NO
    , Password
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AuthTypeValue = AuthTypeValue' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern NO :: AuthTypeValue
pattern NO = AuthTypeValue' "no"

pattern Password :: AuthTypeValue
pattern Password = AuthTypeValue' "password"

{-# COMPLETE
  NO,
  Password,
  AuthTypeValue' #-}

instance FromText AuthTypeValue where
    parser = (AuthTypeValue' . mk) <$> takeText

instance ToText AuthTypeValue where
    toText (AuthTypeValue' ci) = original ci

-- | Represents an enum of /known/ $AuthTypeValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AuthTypeValue where
    toEnum i = case i of
        0 -> NO
        1 -> Password
        _ -> (error . showText) $ "Unknown index for AuthTypeValue: " <> toText i
    fromEnum x = case x of
        NO -> 0
        Password -> 1
        AuthTypeValue' name -> (error . showText) $ "Unknown AuthTypeValue: " <> original name

-- | Represents the bounds of /known/ $AuthTypeValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AuthTypeValue where
    minBound = NO
    maxBound = Password

instance Hashable     AuthTypeValue
instance NFData       AuthTypeValue
instance ToByteString AuthTypeValue
instance ToQuery      AuthTypeValue
instance ToHeader     AuthTypeValue

instance ToJSON AuthTypeValue where
    toJSON = toJSONText

instance FromJSON AuthTypeValue where
    parseJSON = parseJSONText "AuthTypeValue"
