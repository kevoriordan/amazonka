{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.ClusterState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Snowball.Types.ClusterState (
  ClusterState (
    ..
    , AwaitingQuorum
    , Cancelled
    , Complete
    , InUse
    , Pending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ClusterState = ClusterState' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern AwaitingQuorum :: ClusterState
pattern AwaitingQuorum = ClusterState' "AwaitingQuorum"

pattern Cancelled :: ClusterState
pattern Cancelled = ClusterState' "Cancelled"

pattern Complete :: ClusterState
pattern Complete = ClusterState' "Complete"

pattern InUse :: ClusterState
pattern InUse = ClusterState' "InUse"

pattern Pending :: ClusterState
pattern Pending = ClusterState' "Pending"

{-# COMPLETE
  AwaitingQuorum,
  Cancelled,
  Complete,
  InUse,
  Pending,
  ClusterState' #-}

instance FromText ClusterState where
    parser = (ClusterState' . mk) <$> takeText

instance ToText ClusterState where
    toText (ClusterState' ci) = original ci

-- | Represents an enum of /known/ $ClusterState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ClusterState where
    toEnum i = case i of
        0 -> AwaitingQuorum
        1 -> Cancelled
        2 -> Complete
        3 -> InUse
        4 -> Pending
        _ -> (error . showText) $ "Unknown index for ClusterState: " <> toText i
    fromEnum x = case x of
        AwaitingQuorum -> 0
        Cancelled -> 1
        Complete -> 2
        InUse -> 3
        Pending -> 4
        ClusterState' name -> (error . showText) $ "Unknown ClusterState: " <> original name

-- | Represents the bounds of /known/ $ClusterState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ClusterState where
    minBound = AwaitingQuorum
    maxBound = Pending

instance Hashable     ClusterState
instance NFData       ClusterState
instance ToByteString ClusterState
instance ToQuery      ClusterState
instance ToHeader     ClusterState

instance FromJSON ClusterState where
    parseJSON = parseJSONText "ClusterState"
