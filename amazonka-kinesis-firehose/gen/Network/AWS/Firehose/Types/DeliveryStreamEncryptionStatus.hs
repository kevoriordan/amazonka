{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.DeliveryStreamEncryptionStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.DeliveryStreamEncryptionStatus (
  DeliveryStreamEncryptionStatus (
    ..
    , DSESDisabled
    , DSESDisabling
    , DSESDisablingFailed
    , DSESEnabled
    , DSESEnabling
    , DSESEnablingFailed
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DeliveryStreamEncryptionStatus = DeliveryStreamEncryptionStatus' (CI
                                                                         Text)
                                        deriving (Eq, Ord, Read, Show, Data,
                                                  Typeable, Generic)

pattern DSESDisabled :: DeliveryStreamEncryptionStatus
pattern DSESDisabled = DeliveryStreamEncryptionStatus' "DISABLED"

pattern DSESDisabling :: DeliveryStreamEncryptionStatus
pattern DSESDisabling = DeliveryStreamEncryptionStatus' "DISABLING"

pattern DSESDisablingFailed :: DeliveryStreamEncryptionStatus
pattern DSESDisablingFailed = DeliveryStreamEncryptionStatus' "DISABLING_FAILED"

pattern DSESEnabled :: DeliveryStreamEncryptionStatus
pattern DSESEnabled = DeliveryStreamEncryptionStatus' "ENABLED"

pattern DSESEnabling :: DeliveryStreamEncryptionStatus
pattern DSESEnabling = DeliveryStreamEncryptionStatus' "ENABLING"

pattern DSESEnablingFailed :: DeliveryStreamEncryptionStatus
pattern DSESEnablingFailed = DeliveryStreamEncryptionStatus' "ENABLING_FAILED"

{-# COMPLETE
  DSESDisabled,
  DSESDisabling,
  DSESDisablingFailed,
  DSESEnabled,
  DSESEnabling,
  DSESEnablingFailed,
  DeliveryStreamEncryptionStatus' #-}

instance FromText DeliveryStreamEncryptionStatus where
    parser = (DeliveryStreamEncryptionStatus' . mk) <$> takeText

instance ToText DeliveryStreamEncryptionStatus where
    toText (DeliveryStreamEncryptionStatus' ci) = original ci

-- | Represents an enum of /known/ $DeliveryStreamEncryptionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DeliveryStreamEncryptionStatus where
    toEnum i = case i of
        0 -> DSESDisabled
        1 -> DSESDisabling
        2 -> DSESDisablingFailed
        3 -> DSESEnabled
        4 -> DSESEnabling
        5 -> DSESEnablingFailed
        _ -> (error . showText) $ "Unknown index for DeliveryStreamEncryptionStatus: " <> toText i
    fromEnum x = case x of
        DSESDisabled -> 0
        DSESDisabling -> 1
        DSESDisablingFailed -> 2
        DSESEnabled -> 3
        DSESEnabling -> 4
        DSESEnablingFailed -> 5
        DeliveryStreamEncryptionStatus' name -> (error . showText) $ "Unknown DeliveryStreamEncryptionStatus: " <> original name

-- | Represents the bounds of /known/ $DeliveryStreamEncryptionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DeliveryStreamEncryptionStatus where
    minBound = DSESDisabled
    maxBound = DSESEnablingFailed

instance Hashable     DeliveryStreamEncryptionStatus
instance NFData       DeliveryStreamEncryptionStatus
instance ToByteString DeliveryStreamEncryptionStatus
instance ToQuery      DeliveryStreamEncryptionStatus
instance ToHeader     DeliveryStreamEncryptionStatus

instance FromJSON DeliveryStreamEncryptionStatus where
    parseJSON = parseJSONText "DeliveryStreamEncryptionStatus"
