{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ReservationState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.ReservationState (
  ReservationState (
    ..
    , RSActive
    , RSCanceled
    , RSDeleted
    , RSExpired
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Current reservation state
data ReservationState = ReservationState' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern RSActive :: ReservationState
pattern RSActive = ReservationState' "ACTIVE"

pattern RSCanceled :: ReservationState
pattern RSCanceled = ReservationState' "CANCELED"

pattern RSDeleted :: ReservationState
pattern RSDeleted = ReservationState' "DELETED"

pattern RSExpired :: ReservationState
pattern RSExpired = ReservationState' "EXPIRED"

{-# COMPLETE
  RSActive,
  RSCanceled,
  RSDeleted,
  RSExpired,
  ReservationState' #-}

instance FromText ReservationState where
    parser = (ReservationState' . mk) <$> takeText

instance ToText ReservationState where
    toText (ReservationState' ci) = original ci

-- | Represents an enum of /known/ $ReservationState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReservationState where
    toEnum i = case i of
        0 -> RSActive
        1 -> RSCanceled
        2 -> RSDeleted
        3 -> RSExpired
        _ -> (error . showText) $ "Unknown index for ReservationState: " <> toText i
    fromEnum x = case x of
        RSActive -> 0
        RSCanceled -> 1
        RSDeleted -> 2
        RSExpired -> 3
        ReservationState' name -> (error . showText) $ "Unknown ReservationState: " <> original name

-- | Represents the bounds of /known/ $ReservationState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReservationState where
    minBound = RSActive
    maxBound = RSExpired

instance Hashable     ReservationState
instance NFData       ReservationState
instance ToByteString ReservationState
instance ToQuery      ReservationState
instance ToHeader     ReservationState

instance FromJSON ReservationState where
    parseJSON = parseJSONText "ReservationState"
