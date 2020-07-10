{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.LagState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.LagState (
  LagState (
    ..
    , LSAvailable
    , LSDeleted
    , LSDeleting
    , LSDown
    , LSPending
    , LSRequested
    , LSUnknown
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LagState = LagState' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern LSAvailable :: LagState
pattern LSAvailable = LagState' "available"

pattern LSDeleted :: LagState
pattern LSDeleted = LagState' "deleted"

pattern LSDeleting :: LagState
pattern LSDeleting = LagState' "deleting"

pattern LSDown :: LagState
pattern LSDown = LagState' "down"

pattern LSPending :: LagState
pattern LSPending = LagState' "pending"

pattern LSRequested :: LagState
pattern LSRequested = LagState' "requested"

pattern LSUnknown :: LagState
pattern LSUnknown = LagState' "unknown"

{-# COMPLETE
  LSAvailable,
  LSDeleted,
  LSDeleting,
  LSDown,
  LSPending,
  LSRequested,
  LSUnknown,
  LagState' #-}

instance FromText LagState where
    parser = (LagState' . mk) <$> takeText

instance ToText LagState where
    toText (LagState' ci) = original ci

-- | Represents an enum of /known/ $LagState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LagState where
    toEnum i = case i of
        0 -> LSAvailable
        1 -> LSDeleted
        2 -> LSDeleting
        3 -> LSDown
        4 -> LSPending
        5 -> LSRequested
        6 -> LSUnknown
        _ -> (error . showText) $ "Unknown index for LagState: " <> toText i
    fromEnum x = case x of
        LSAvailable -> 0
        LSDeleted -> 1
        LSDeleting -> 2
        LSDown -> 3
        LSPending -> 4
        LSRequested -> 5
        LSUnknown -> 6
        LagState' name -> (error . showText) $ "Unknown LagState: " <> original name

-- | Represents the bounds of /known/ $LagState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LagState where
    minBound = LSAvailable
    maxBound = LSUnknown

instance Hashable     LagState
instance NFData       LagState
instance ToByteString LagState
instance ToQuery      LagState
instance ToHeader     LagState

instance FromJSON LagState where
    parseJSON = parseJSONText "LagState"
