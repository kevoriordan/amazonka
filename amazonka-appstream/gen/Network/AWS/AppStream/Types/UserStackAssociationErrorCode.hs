{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.UserStackAssociationErrorCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.UserStackAssociationErrorCode (
  UserStackAssociationErrorCode (
    ..
    , USAECInternalError
    , USAECStackNotFound
    , USAECUserNameNotFound
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data UserStackAssociationErrorCode = UserStackAssociationErrorCode' (CI
                                                                       Text)
                                       deriving (Eq, Ord, Read, Show, Data,
                                                 Typeable, Generic)

pattern USAECInternalError :: UserStackAssociationErrorCode
pattern USAECInternalError = UserStackAssociationErrorCode' "INTERNAL_ERROR"

pattern USAECStackNotFound :: UserStackAssociationErrorCode
pattern USAECStackNotFound = UserStackAssociationErrorCode' "STACK_NOT_FOUND"

pattern USAECUserNameNotFound :: UserStackAssociationErrorCode
pattern USAECUserNameNotFound = UserStackAssociationErrorCode' "USER_NAME_NOT_FOUND"

{-# COMPLETE
  USAECInternalError,
  USAECStackNotFound,
  USAECUserNameNotFound,
  UserStackAssociationErrorCode' #-}

instance FromText UserStackAssociationErrorCode where
    parser = (UserStackAssociationErrorCode' . mk) <$> takeText

instance ToText UserStackAssociationErrorCode where
    toText (UserStackAssociationErrorCode' ci) = original ci

-- | Represents an enum of /known/ $UserStackAssociationErrorCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum UserStackAssociationErrorCode where
    toEnum i = case i of
        0 -> USAECInternalError
        1 -> USAECStackNotFound
        2 -> USAECUserNameNotFound
        _ -> (error . showText) $ "Unknown index for UserStackAssociationErrorCode: " <> toText i
    fromEnum x = case x of
        USAECInternalError -> 0
        USAECStackNotFound -> 1
        USAECUserNameNotFound -> 2
        UserStackAssociationErrorCode' name -> (error . showText) $ "Unknown UserStackAssociationErrorCode: " <> original name

-- | Represents the bounds of /known/ $UserStackAssociationErrorCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded UserStackAssociationErrorCode where
    minBound = USAECInternalError
    maxBound = USAECUserNameNotFound

instance Hashable     UserStackAssociationErrorCode
instance NFData       UserStackAssociationErrorCode
instance ToByteString UserStackAssociationErrorCode
instance ToQuery      UserStackAssociationErrorCode
instance ToHeader     UserStackAssociationErrorCode

instance FromJSON UserStackAssociationErrorCode where
    parseJSON = parseJSONText "UserStackAssociationErrorCode"
