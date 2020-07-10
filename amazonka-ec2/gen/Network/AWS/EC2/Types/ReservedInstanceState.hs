{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservedInstanceState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ReservedInstanceState (
  ReservedInstanceState (
    ..
    , Active
    , PaymentFailed
    , PaymentPending
    , Queued
    , QueuedDeleted
    , Retired
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ReservedInstanceState = ReservedInstanceState' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern Active :: ReservedInstanceState
pattern Active = ReservedInstanceState' "active"

pattern PaymentFailed :: ReservedInstanceState
pattern PaymentFailed = ReservedInstanceState' "payment-failed"

pattern PaymentPending :: ReservedInstanceState
pattern PaymentPending = ReservedInstanceState' "payment-pending"

pattern Queued :: ReservedInstanceState
pattern Queued = ReservedInstanceState' "queued"

pattern QueuedDeleted :: ReservedInstanceState
pattern QueuedDeleted = ReservedInstanceState' "queued-deleted"

pattern Retired :: ReservedInstanceState
pattern Retired = ReservedInstanceState' "retired"

{-# COMPLETE
  Active,
  PaymentFailed,
  PaymentPending,
  Queued,
  QueuedDeleted,
  Retired,
  ReservedInstanceState' #-}

instance FromText ReservedInstanceState where
    parser = (ReservedInstanceState' . mk) <$> takeText

instance ToText ReservedInstanceState where
    toText (ReservedInstanceState' ci) = original ci

-- | Represents an enum of /known/ $ReservedInstanceState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReservedInstanceState where
    toEnum i = case i of
        0 -> Active
        1 -> PaymentFailed
        2 -> PaymentPending
        3 -> Queued
        4 -> QueuedDeleted
        5 -> Retired
        _ -> (error . showText) $ "Unknown index for ReservedInstanceState: " <> toText i
    fromEnum x = case x of
        Active -> 0
        PaymentFailed -> 1
        PaymentPending -> 2
        Queued -> 3
        QueuedDeleted -> 4
        Retired -> 5
        ReservedInstanceState' name -> (error . showText) $ "Unknown ReservedInstanceState: " <> original name

-- | Represents the bounds of /known/ $ReservedInstanceState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReservedInstanceState where
    minBound = Active
    maxBound = Retired

instance Hashable     ReservedInstanceState
instance NFData       ReservedInstanceState
instance ToByteString ReservedInstanceState
instance ToQuery      ReservedInstanceState
instance ToHeader     ReservedInstanceState

instance FromXML ReservedInstanceState where
    parseXML = parseXMLText "ReservedInstanceState"
