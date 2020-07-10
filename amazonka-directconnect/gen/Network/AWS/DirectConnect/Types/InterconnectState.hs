{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.InterconnectState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.InterconnectState (
  InterconnectState (
    ..
    , ISAvailable
    , ISDeleted
    , ISDeleting
    , ISDown
    , ISPending
    , ISRequested
    , ISUnknown
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data InterconnectState = InterconnectState' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern ISAvailable :: InterconnectState
pattern ISAvailable = InterconnectState' "available"

pattern ISDeleted :: InterconnectState
pattern ISDeleted = InterconnectState' "deleted"

pattern ISDeleting :: InterconnectState
pattern ISDeleting = InterconnectState' "deleting"

pattern ISDown :: InterconnectState
pattern ISDown = InterconnectState' "down"

pattern ISPending :: InterconnectState
pattern ISPending = InterconnectState' "pending"

pattern ISRequested :: InterconnectState
pattern ISRequested = InterconnectState' "requested"

pattern ISUnknown :: InterconnectState
pattern ISUnknown = InterconnectState' "unknown"

{-# COMPLETE
  ISAvailable,
  ISDeleted,
  ISDeleting,
  ISDown,
  ISPending,
  ISRequested,
  ISUnknown,
  InterconnectState' #-}

instance FromText InterconnectState where
    parser = (InterconnectState' . mk) <$> takeText

instance ToText InterconnectState where
    toText (InterconnectState' ci) = original ci

-- | Represents an enum of /known/ $InterconnectState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InterconnectState where
    toEnum i = case i of
        0 -> ISAvailable
        1 -> ISDeleted
        2 -> ISDeleting
        3 -> ISDown
        4 -> ISPending
        5 -> ISRequested
        6 -> ISUnknown
        _ -> (error . showText) $ "Unknown index for InterconnectState: " <> toText i
    fromEnum x = case x of
        ISAvailable -> 0
        ISDeleted -> 1
        ISDeleting -> 2
        ISDown -> 3
        ISPending -> 4
        ISRequested -> 5
        ISUnknown -> 6
        InterconnectState' name -> (error . showText) $ "Unknown InterconnectState: " <> original name

-- | Represents the bounds of /known/ $InterconnectState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InterconnectState where
    minBound = ISAvailable
    maxBound = ISUnknown

instance Hashable     InterconnectState
instance NFData       InterconnectState
instance ToByteString InterconnectState
instance ToQuery      InterconnectState
instance ToHeader     InterconnectState

instance FromJSON InterconnectState where
    parseJSON = parseJSONText "InterconnectState"
