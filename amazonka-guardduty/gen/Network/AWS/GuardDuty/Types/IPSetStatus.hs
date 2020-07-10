{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.IPSetStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.IPSetStatus (
  IPSetStatus (
    ..
    , ISSActivating
    , ISSActive
    , ISSDeactivating
    , ISSDeletePending
    , ISSDeleted
    , ISSError'
    , ISSInactive
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data IPSetStatus = IPSetStatus' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern ISSActivating :: IPSetStatus
pattern ISSActivating = IPSetStatus' "ACTIVATING"

pattern ISSActive :: IPSetStatus
pattern ISSActive = IPSetStatus' "ACTIVE"

pattern ISSDeactivating :: IPSetStatus
pattern ISSDeactivating = IPSetStatus' "DEACTIVATING"

pattern ISSDeletePending :: IPSetStatus
pattern ISSDeletePending = IPSetStatus' "DELETE_PENDING"

pattern ISSDeleted :: IPSetStatus
pattern ISSDeleted = IPSetStatus' "DELETED"

pattern ISSError' :: IPSetStatus
pattern ISSError' = IPSetStatus' "ERROR"

pattern ISSInactive :: IPSetStatus
pattern ISSInactive = IPSetStatus' "INACTIVE"

{-# COMPLETE
  ISSActivating,
  ISSActive,
  ISSDeactivating,
  ISSDeletePending,
  ISSDeleted,
  ISSError',
  ISSInactive,
  IPSetStatus' #-}

instance FromText IPSetStatus where
    parser = (IPSetStatus' . mk) <$> takeText

instance ToText IPSetStatus where
    toText (IPSetStatus' ci) = original ci

-- | Represents an enum of /known/ $IPSetStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum IPSetStatus where
    toEnum i = case i of
        0 -> ISSActivating
        1 -> ISSActive
        2 -> ISSDeactivating
        3 -> ISSDeletePending
        4 -> ISSDeleted
        5 -> ISSError'
        6 -> ISSInactive
        _ -> (error . showText) $ "Unknown index for IPSetStatus: " <> toText i
    fromEnum x = case x of
        ISSActivating -> 0
        ISSActive -> 1
        ISSDeactivating -> 2
        ISSDeletePending -> 3
        ISSDeleted -> 4
        ISSError' -> 5
        ISSInactive -> 6
        IPSetStatus' name -> (error . showText) $ "Unknown IPSetStatus: " <> original name

-- | Represents the bounds of /known/ $IPSetStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded IPSetStatus where
    minBound = ISSActivating
    maxBound = ISSInactive

instance Hashable     IPSetStatus
instance NFData       IPSetStatus
instance ToByteString IPSetStatus
instance ToQuery      IPSetStatus
instance ToHeader     IPSetStatus

instance FromJSON IPSetStatus where
    parseJSON = parseJSONText "IPSetStatus"
