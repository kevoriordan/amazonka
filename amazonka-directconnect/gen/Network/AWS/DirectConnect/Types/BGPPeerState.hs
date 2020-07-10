{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.BGPPeerState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.BGPPeerState (
  BGPPeerState (
    ..
    , Available
    , Deleted
    , Deleting
    , Pending
    , Verifying
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data BGPPeerState = BGPPeerState' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Available :: BGPPeerState
pattern Available = BGPPeerState' "available"

pattern Deleted :: BGPPeerState
pattern Deleted = BGPPeerState' "deleted"

pattern Deleting :: BGPPeerState
pattern Deleting = BGPPeerState' "deleting"

pattern Pending :: BGPPeerState
pattern Pending = BGPPeerState' "pending"

pattern Verifying :: BGPPeerState
pattern Verifying = BGPPeerState' "verifying"

{-# COMPLETE
  Available,
  Deleted,
  Deleting,
  Pending,
  Verifying,
  BGPPeerState' #-}

instance FromText BGPPeerState where
    parser = (BGPPeerState' . mk) <$> takeText

instance ToText BGPPeerState where
    toText (BGPPeerState' ci) = original ci

-- | Represents an enum of /known/ $BGPPeerState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BGPPeerState where
    toEnum i = case i of
        0 -> Available
        1 -> Deleted
        2 -> Deleting
        3 -> Pending
        4 -> Verifying
        _ -> (error . showText) $ "Unknown index for BGPPeerState: " <> toText i
    fromEnum x = case x of
        Available -> 0
        Deleted -> 1
        Deleting -> 2
        Pending -> 3
        Verifying -> 4
        BGPPeerState' name -> (error . showText) $ "Unknown BGPPeerState: " <> original name

-- | Represents the bounds of /known/ $BGPPeerState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BGPPeerState where
    minBound = Available
    maxBound = Verifying

instance Hashable     BGPPeerState
instance NFData       BGPPeerState
instance ToByteString BGPPeerState
instance ToQuery      BGPPeerState
instance ToHeader     BGPPeerState

instance FromJSON BGPPeerState where
    parseJSON = parseJSONText "BGPPeerState"
