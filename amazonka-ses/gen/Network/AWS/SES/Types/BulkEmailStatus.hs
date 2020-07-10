{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.BulkEmailStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.BulkEmailStatus (
  BulkEmailStatus (
    ..
    , AccountDailyQuotaExceeded
    , AccountSendingPaused
    , AccountSuspended
    , AccountThrottled
    , ConfigurationSetDoesNotExist
    , ConfigurationSetSendingPaused
    , Failed
    , InvalidParameterValue
    , InvalidSendingPoolName
    , MailFromDomainNotVerified
    , MessageRejected
    , Success
    , TemplateDoesNotExist
    , TransientFailure
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data BulkEmailStatus = BulkEmailStatus' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern AccountDailyQuotaExceeded :: BulkEmailStatus
pattern AccountDailyQuotaExceeded = BulkEmailStatus' "AccountDailyQuotaExceeded"

pattern AccountSendingPaused :: BulkEmailStatus
pattern AccountSendingPaused = BulkEmailStatus' "AccountSendingPaused"

pattern AccountSuspended :: BulkEmailStatus
pattern AccountSuspended = BulkEmailStatus' "AccountSuspended"

pattern AccountThrottled :: BulkEmailStatus
pattern AccountThrottled = BulkEmailStatus' "AccountThrottled"

pattern ConfigurationSetDoesNotExist :: BulkEmailStatus
pattern ConfigurationSetDoesNotExist = BulkEmailStatus' "ConfigurationSetDoesNotExist"

pattern ConfigurationSetSendingPaused :: BulkEmailStatus
pattern ConfigurationSetSendingPaused = BulkEmailStatus' "ConfigurationSetSendingPaused"

pattern Failed :: BulkEmailStatus
pattern Failed = BulkEmailStatus' "Failed"

pattern InvalidParameterValue :: BulkEmailStatus
pattern InvalidParameterValue = BulkEmailStatus' "InvalidParameterValue"

pattern InvalidSendingPoolName :: BulkEmailStatus
pattern InvalidSendingPoolName = BulkEmailStatus' "InvalidSendingPoolName"

pattern MailFromDomainNotVerified :: BulkEmailStatus
pattern MailFromDomainNotVerified = BulkEmailStatus' "MailFromDomainNotVerified"

pattern MessageRejected :: BulkEmailStatus
pattern MessageRejected = BulkEmailStatus' "MessageRejected"

pattern Success :: BulkEmailStatus
pattern Success = BulkEmailStatus' "Success"

pattern TemplateDoesNotExist :: BulkEmailStatus
pattern TemplateDoesNotExist = BulkEmailStatus' "TemplateDoesNotExist"

pattern TransientFailure :: BulkEmailStatus
pattern TransientFailure = BulkEmailStatus' "TransientFailure"

{-# COMPLETE
  AccountDailyQuotaExceeded,
  AccountSendingPaused,
  AccountSuspended,
  AccountThrottled,
  ConfigurationSetDoesNotExist,
  ConfigurationSetSendingPaused,
  Failed,
  InvalidParameterValue,
  InvalidSendingPoolName,
  MailFromDomainNotVerified,
  MessageRejected,
  Success,
  TemplateDoesNotExist,
  TransientFailure,
  BulkEmailStatus' #-}

instance FromText BulkEmailStatus where
    parser = (BulkEmailStatus' . mk) <$> takeText

instance ToText BulkEmailStatus where
    toText (BulkEmailStatus' ci) = original ci

-- | Represents an enum of /known/ $BulkEmailStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BulkEmailStatus where
    toEnum i = case i of
        0 -> AccountDailyQuotaExceeded
        1 -> AccountSendingPaused
        2 -> AccountSuspended
        3 -> AccountThrottled
        4 -> ConfigurationSetDoesNotExist
        5 -> ConfigurationSetSendingPaused
        6 -> Failed
        7 -> InvalidParameterValue
        8 -> InvalidSendingPoolName
        9 -> MailFromDomainNotVerified
        10 -> MessageRejected
        11 -> Success
        12 -> TemplateDoesNotExist
        13 -> TransientFailure
        _ -> (error . showText) $ "Unknown index for BulkEmailStatus: " <> toText i
    fromEnum x = case x of
        AccountDailyQuotaExceeded -> 0
        AccountSendingPaused -> 1
        AccountSuspended -> 2
        AccountThrottled -> 3
        ConfigurationSetDoesNotExist -> 4
        ConfigurationSetSendingPaused -> 5
        Failed -> 6
        InvalidParameterValue -> 7
        InvalidSendingPoolName -> 8
        MailFromDomainNotVerified -> 9
        MessageRejected -> 10
        Success -> 11
        TemplateDoesNotExist -> 12
        TransientFailure -> 13
        BulkEmailStatus' name -> (error . showText) $ "Unknown BulkEmailStatus: " <> original name

-- | Represents the bounds of /known/ $BulkEmailStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BulkEmailStatus where
    minBound = AccountDailyQuotaExceeded
    maxBound = TransientFailure

instance Hashable     BulkEmailStatus
instance NFData       BulkEmailStatus
instance ToByteString BulkEmailStatus
instance ToQuery      BulkEmailStatus
instance ToHeader     BulkEmailStatus

instance FromXML BulkEmailStatus where
    parseXML = parseXMLText "BulkEmailStatus"
