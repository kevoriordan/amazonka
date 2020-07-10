{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.AccountGateStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.AccountGateStatus (
  AccountGateStatus (
    ..
    , AGSFailed
    , AGSSkipped
    , AGSSucceeded
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AccountGateStatus = AccountGateStatus' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern AGSFailed :: AccountGateStatus
pattern AGSFailed = AccountGateStatus' "FAILED"

pattern AGSSkipped :: AccountGateStatus
pattern AGSSkipped = AccountGateStatus' "SKIPPED"

pattern AGSSucceeded :: AccountGateStatus
pattern AGSSucceeded = AccountGateStatus' "SUCCEEDED"

{-# COMPLETE
  AGSFailed,
  AGSSkipped,
  AGSSucceeded,
  AccountGateStatus' #-}

instance FromText AccountGateStatus where
    parser = (AccountGateStatus' . mk) <$> takeText

instance ToText AccountGateStatus where
    toText (AccountGateStatus' ci) = original ci

-- | Represents an enum of /known/ $AccountGateStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AccountGateStatus where
    toEnum i = case i of
        0 -> AGSFailed
        1 -> AGSSkipped
        2 -> AGSSucceeded
        _ -> (error . showText) $ "Unknown index for AccountGateStatus: " <> toText i
    fromEnum x = case x of
        AGSFailed -> 0
        AGSSkipped -> 1
        AGSSucceeded -> 2
        AccountGateStatus' name -> (error . showText) $ "Unknown AccountGateStatus: " <> original name

-- | Represents the bounds of /known/ $AccountGateStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AccountGateStatus where
    minBound = AGSFailed
    maxBound = AGSSucceeded

instance Hashable     AccountGateStatus
instance NFData       AccountGateStatus
instance ToByteString AccountGateStatus
instance ToQuery      AccountGateStatus
instance ToHeader     AccountGateStatus

instance FromXML AccountGateStatus where
    parseXML = parseXMLText "AccountGateStatus"
