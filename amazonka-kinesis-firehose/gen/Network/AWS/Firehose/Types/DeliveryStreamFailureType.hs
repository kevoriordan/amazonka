{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.DeliveryStreamFailureType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.DeliveryStreamFailureType (
  DeliveryStreamFailureType (
    ..
    , CreateEniFailed
    , CreateKMSGrantFailed
    , DeleteEniFailed
    , DisabledKMSKey
    , EniAccessDenied
    , InvalidKMSKey
    , KMSAccessDenied
    , KMSKeyNotFound
    , KMSOptInRequired
    , RetireKMSGrantFailed
    , SecurityGroupAccessDenied
    , SecurityGroupNotFound
    , SubnetAccessDenied
    , SubnetNotFound
    , UnknownError
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DeliveryStreamFailureType = DeliveryStreamFailureType' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern CreateEniFailed :: DeliveryStreamFailureType
pattern CreateEniFailed = DeliveryStreamFailureType' "CREATE_ENI_FAILED"

pattern CreateKMSGrantFailed :: DeliveryStreamFailureType
pattern CreateKMSGrantFailed = DeliveryStreamFailureType' "CREATE_KMS_GRANT_FAILED"

pattern DeleteEniFailed :: DeliveryStreamFailureType
pattern DeleteEniFailed = DeliveryStreamFailureType' "DELETE_ENI_FAILED"

pattern DisabledKMSKey :: DeliveryStreamFailureType
pattern DisabledKMSKey = DeliveryStreamFailureType' "DISABLED_KMS_KEY"

pattern EniAccessDenied :: DeliveryStreamFailureType
pattern EniAccessDenied = DeliveryStreamFailureType' "ENI_ACCESS_DENIED"

pattern InvalidKMSKey :: DeliveryStreamFailureType
pattern InvalidKMSKey = DeliveryStreamFailureType' "INVALID_KMS_KEY"

pattern KMSAccessDenied :: DeliveryStreamFailureType
pattern KMSAccessDenied = DeliveryStreamFailureType' "KMS_ACCESS_DENIED"

pattern KMSKeyNotFound :: DeliveryStreamFailureType
pattern KMSKeyNotFound = DeliveryStreamFailureType' "KMS_KEY_NOT_FOUND"

pattern KMSOptInRequired :: DeliveryStreamFailureType
pattern KMSOptInRequired = DeliveryStreamFailureType' "KMS_OPT_IN_REQUIRED"

pattern RetireKMSGrantFailed :: DeliveryStreamFailureType
pattern RetireKMSGrantFailed = DeliveryStreamFailureType' "RETIRE_KMS_GRANT_FAILED"

pattern SecurityGroupAccessDenied :: DeliveryStreamFailureType
pattern SecurityGroupAccessDenied = DeliveryStreamFailureType' "SECURITY_GROUP_ACCESS_DENIED"

pattern SecurityGroupNotFound :: DeliveryStreamFailureType
pattern SecurityGroupNotFound = DeliveryStreamFailureType' "SECURITY_GROUP_NOT_FOUND"

pattern SubnetAccessDenied :: DeliveryStreamFailureType
pattern SubnetAccessDenied = DeliveryStreamFailureType' "SUBNET_ACCESS_DENIED"

pattern SubnetNotFound :: DeliveryStreamFailureType
pattern SubnetNotFound = DeliveryStreamFailureType' "SUBNET_NOT_FOUND"

pattern UnknownError :: DeliveryStreamFailureType
pattern UnknownError = DeliveryStreamFailureType' "UNKNOWN_ERROR"

{-# COMPLETE
  CreateEniFailed,
  CreateKMSGrantFailed,
  DeleteEniFailed,
  DisabledKMSKey,
  EniAccessDenied,
  InvalidKMSKey,
  KMSAccessDenied,
  KMSKeyNotFound,
  KMSOptInRequired,
  RetireKMSGrantFailed,
  SecurityGroupAccessDenied,
  SecurityGroupNotFound,
  SubnetAccessDenied,
  SubnetNotFound,
  UnknownError,
  DeliveryStreamFailureType' #-}

instance FromText DeliveryStreamFailureType where
    parser = (DeliveryStreamFailureType' . mk) <$> takeText

instance ToText DeliveryStreamFailureType where
    toText (DeliveryStreamFailureType' ci) = original ci

-- | Represents an enum of /known/ $DeliveryStreamFailureType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DeliveryStreamFailureType where
    toEnum i = case i of
        0 -> CreateEniFailed
        1 -> CreateKMSGrantFailed
        2 -> DeleteEniFailed
        3 -> DisabledKMSKey
        4 -> EniAccessDenied
        5 -> InvalidKMSKey
        6 -> KMSAccessDenied
        7 -> KMSKeyNotFound
        8 -> KMSOptInRequired
        9 -> RetireKMSGrantFailed
        10 -> SecurityGroupAccessDenied
        11 -> SecurityGroupNotFound
        12 -> SubnetAccessDenied
        13 -> SubnetNotFound
        14 -> UnknownError
        _ -> (error . showText) $ "Unknown index for DeliveryStreamFailureType: " <> toText i
    fromEnum x = case x of
        CreateEniFailed -> 0
        CreateKMSGrantFailed -> 1
        DeleteEniFailed -> 2
        DisabledKMSKey -> 3
        EniAccessDenied -> 4
        InvalidKMSKey -> 5
        KMSAccessDenied -> 6
        KMSKeyNotFound -> 7
        KMSOptInRequired -> 8
        RetireKMSGrantFailed -> 9
        SecurityGroupAccessDenied -> 10
        SecurityGroupNotFound -> 11
        SubnetAccessDenied -> 12
        SubnetNotFound -> 13
        UnknownError -> 14
        DeliveryStreamFailureType' name -> (error . showText) $ "Unknown DeliveryStreamFailureType: " <> original name

-- | Represents the bounds of /known/ $DeliveryStreamFailureType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DeliveryStreamFailureType where
    minBound = CreateEniFailed
    maxBound = UnknownError

instance Hashable     DeliveryStreamFailureType
instance NFData       DeliveryStreamFailureType
instance ToByteString DeliveryStreamFailureType
instance ToQuery      DeliveryStreamFailureType
instance ToHeader     DeliveryStreamFailureType

instance FromJSON DeliveryStreamFailureType where
    parseJSON = parseJSONText "DeliveryStreamFailureType"
