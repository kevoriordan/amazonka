{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.BrokerState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MQ.Types.BrokerState (
  BrokerState (
    ..
    , CreationFailed
    , CreationInProgress
    , DeletionInProgress
    , RebootInProgress
    , Running
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The status of the broker.
data BrokerState = BrokerState' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern CreationFailed :: BrokerState
pattern CreationFailed = BrokerState' "CREATION_FAILED"

pattern CreationInProgress :: BrokerState
pattern CreationInProgress = BrokerState' "CREATION_IN_PROGRESS"

pattern DeletionInProgress :: BrokerState
pattern DeletionInProgress = BrokerState' "DELETION_IN_PROGRESS"

pattern RebootInProgress :: BrokerState
pattern RebootInProgress = BrokerState' "REBOOT_IN_PROGRESS"

pattern Running :: BrokerState
pattern Running = BrokerState' "RUNNING"

{-# COMPLETE
  CreationFailed,
  CreationInProgress,
  DeletionInProgress,
  RebootInProgress,
  Running,
  BrokerState' #-}

instance FromText BrokerState where
    parser = (BrokerState' . mk) <$> takeText

instance ToText BrokerState where
    toText (BrokerState' ci) = original ci

-- | Represents an enum of /known/ $BrokerState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BrokerState where
    toEnum i = case i of
        0 -> CreationFailed
        1 -> CreationInProgress
        2 -> DeletionInProgress
        3 -> RebootInProgress
        4 -> Running
        _ -> (error . showText) $ "Unknown index for BrokerState: " <> toText i
    fromEnum x = case x of
        CreationFailed -> 0
        CreationInProgress -> 1
        DeletionInProgress -> 2
        RebootInProgress -> 3
        Running -> 4
        BrokerState' name -> (error . showText) $ "Unknown BrokerState: " <> original name

-- | Represents the bounds of /known/ $BrokerState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BrokerState where
    minBound = CreationFailed
    maxBound = Running

instance Hashable     BrokerState
instance NFData       BrokerState
instance ToByteString BrokerState
instance ToQuery      BrokerState
instance ToHeader     BrokerState

instance FromJSON BrokerState where
    parseJSON = parseJSONText "BrokerState"
