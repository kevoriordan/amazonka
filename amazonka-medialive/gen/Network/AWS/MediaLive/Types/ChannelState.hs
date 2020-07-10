{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ChannelState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.ChannelState (
  ChannelState (
    ..
    , CreateFailed
    , Creating
    , Deleted
    , Deleting
    , Idle
    , Recovering
    , Running
    , Starting
    , Stopping
    , UpdateFailed
    , Updating
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Placeholder documentation for ChannelState
data ChannelState = ChannelState' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern CreateFailed :: ChannelState
pattern CreateFailed = ChannelState' "CREATE_FAILED"

pattern Creating :: ChannelState
pattern Creating = ChannelState' "CREATING"

pattern Deleted :: ChannelState
pattern Deleted = ChannelState' "DELETED"

pattern Deleting :: ChannelState
pattern Deleting = ChannelState' "DELETING"

pattern Idle :: ChannelState
pattern Idle = ChannelState' "IDLE"

pattern Recovering :: ChannelState
pattern Recovering = ChannelState' "RECOVERING"

pattern Running :: ChannelState
pattern Running = ChannelState' "RUNNING"

pattern Starting :: ChannelState
pattern Starting = ChannelState' "STARTING"

pattern Stopping :: ChannelState
pattern Stopping = ChannelState' "STOPPING"

pattern UpdateFailed :: ChannelState
pattern UpdateFailed = ChannelState' "UPDATE_FAILED"

pattern Updating :: ChannelState
pattern Updating = ChannelState' "UPDATING"

{-# COMPLETE
  CreateFailed,
  Creating,
  Deleted,
  Deleting,
  Idle,
  Recovering,
  Running,
  Starting,
  Stopping,
  UpdateFailed,
  Updating,
  ChannelState' #-}

instance FromText ChannelState where
    parser = (ChannelState' . mk) <$> takeText

instance ToText ChannelState where
    toText (ChannelState' ci) = original ci

-- | Represents an enum of /known/ $ChannelState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ChannelState where
    toEnum i = case i of
        0 -> CreateFailed
        1 -> Creating
        2 -> Deleted
        3 -> Deleting
        4 -> Idle
        5 -> Recovering
        6 -> Running
        7 -> Starting
        8 -> Stopping
        9 -> UpdateFailed
        10 -> Updating
        _ -> (error . showText) $ "Unknown index for ChannelState: " <> toText i
    fromEnum x = case x of
        CreateFailed -> 0
        Creating -> 1
        Deleted -> 2
        Deleting -> 3
        Idle -> 4
        Recovering -> 5
        Running -> 6
        Starting -> 7
        Stopping -> 8
        UpdateFailed -> 9
        Updating -> 10
        ChannelState' name -> (error . showText) $ "Unknown ChannelState: " <> original name

-- | Represents the bounds of /known/ $ChannelState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ChannelState where
    minBound = CreateFailed
    maxBound = Updating

instance Hashable     ChannelState
instance NFData       ChannelState
instance ToByteString ChannelState
instance ToQuery      ChannelState
instance ToHeader     ChannelState

instance FromJSON ChannelState where
    parseJSON = parseJSONText "ChannelState"
