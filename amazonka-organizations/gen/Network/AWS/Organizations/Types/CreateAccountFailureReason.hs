{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.CreateAccountFailureReason
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Organizations.Types.CreateAccountFailureReason (
  CreateAccountFailureReason (
    ..
    , AccountLimitExceeded
    , ConcurrentAccountModification
    , EmailAlreadyExists
    , GovcloudAccountAlreadyExists
    , InternalFailure
    , InvalidAddress
    , InvalidEmail
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CreateAccountFailureReason = CreateAccountFailureReason' (CI
                                                                 Text)
                                    deriving (Eq, Ord, Read, Show, Data,
                                              Typeable, Generic)

pattern AccountLimitExceeded :: CreateAccountFailureReason
pattern AccountLimitExceeded = CreateAccountFailureReason' "ACCOUNT_LIMIT_EXCEEDED"

pattern ConcurrentAccountModification :: CreateAccountFailureReason
pattern ConcurrentAccountModification = CreateAccountFailureReason' "CONCURRENT_ACCOUNT_MODIFICATION"

pattern EmailAlreadyExists :: CreateAccountFailureReason
pattern EmailAlreadyExists = CreateAccountFailureReason' "EMAIL_ALREADY_EXISTS"

pattern GovcloudAccountAlreadyExists :: CreateAccountFailureReason
pattern GovcloudAccountAlreadyExists = CreateAccountFailureReason' "GOVCLOUD_ACCOUNT_ALREADY_EXISTS"

pattern InternalFailure :: CreateAccountFailureReason
pattern InternalFailure = CreateAccountFailureReason' "INTERNAL_FAILURE"

pattern InvalidAddress :: CreateAccountFailureReason
pattern InvalidAddress = CreateAccountFailureReason' "INVALID_ADDRESS"

pattern InvalidEmail :: CreateAccountFailureReason
pattern InvalidEmail = CreateAccountFailureReason' "INVALID_EMAIL"

{-# COMPLETE
  AccountLimitExceeded,
  ConcurrentAccountModification,
  EmailAlreadyExists,
  GovcloudAccountAlreadyExists,
  InternalFailure,
  InvalidAddress,
  InvalidEmail,
  CreateAccountFailureReason' #-}

instance FromText CreateAccountFailureReason where
    parser = (CreateAccountFailureReason' . mk) <$> takeText

instance ToText CreateAccountFailureReason where
    toText (CreateAccountFailureReason' ci) = original ci

-- | Represents an enum of /known/ $CreateAccountFailureReason.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CreateAccountFailureReason where
    toEnum i = case i of
        0 -> AccountLimitExceeded
        1 -> ConcurrentAccountModification
        2 -> EmailAlreadyExists
        3 -> GovcloudAccountAlreadyExists
        4 -> InternalFailure
        5 -> InvalidAddress
        6 -> InvalidEmail
        _ -> (error . showText) $ "Unknown index for CreateAccountFailureReason: " <> toText i
    fromEnum x = case x of
        AccountLimitExceeded -> 0
        ConcurrentAccountModification -> 1
        EmailAlreadyExists -> 2
        GovcloudAccountAlreadyExists -> 3
        InternalFailure -> 4
        InvalidAddress -> 5
        InvalidEmail -> 6
        CreateAccountFailureReason' name -> (error . showText) $ "Unknown CreateAccountFailureReason: " <> original name

-- | Represents the bounds of /known/ $CreateAccountFailureReason.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CreateAccountFailureReason where
    minBound = AccountLimitExceeded
    maxBound = InvalidEmail

instance Hashable     CreateAccountFailureReason
instance NFData       CreateAccountFailureReason
instance ToByteString CreateAccountFailureReason
instance ToQuery      CreateAccountFailureReason
instance ToHeader     CreateAccountFailureReason

instance FromJSON CreateAccountFailureReason where
    parseJSON = parseJSONText "CreateAccountFailureReason"
