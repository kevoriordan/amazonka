{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.NodeUpdateStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.NodeUpdateStatus (
  NodeUpdateStatus (
    ..
    , NUSComplete
    , NUSInProgress
    , NUSNotApplied
    , NUSStopped
    , NUSStopping
    , NUSWaitingToStart
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data NodeUpdateStatus = NodeUpdateStatus' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern NUSComplete :: NodeUpdateStatus
pattern NUSComplete = NodeUpdateStatus' "complete"

pattern NUSInProgress :: NodeUpdateStatus
pattern NUSInProgress = NodeUpdateStatus' "in-progress"

pattern NUSNotApplied :: NodeUpdateStatus
pattern NUSNotApplied = NodeUpdateStatus' "not-applied"

pattern NUSStopped :: NodeUpdateStatus
pattern NUSStopped = NodeUpdateStatus' "stopped"

pattern NUSStopping :: NodeUpdateStatus
pattern NUSStopping = NodeUpdateStatus' "stopping"

pattern NUSWaitingToStart :: NodeUpdateStatus
pattern NUSWaitingToStart = NodeUpdateStatus' "waiting-to-start"

{-# COMPLETE
  NUSComplete,
  NUSInProgress,
  NUSNotApplied,
  NUSStopped,
  NUSStopping,
  NUSWaitingToStart,
  NodeUpdateStatus' #-}

instance FromText NodeUpdateStatus where
    parser = (NodeUpdateStatus' . mk) <$> takeText

instance ToText NodeUpdateStatus where
    toText (NodeUpdateStatus' ci) = original ci

-- | Represents an enum of /known/ $NodeUpdateStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum NodeUpdateStatus where
    toEnum i = case i of
        0 -> NUSComplete
        1 -> NUSInProgress
        2 -> NUSNotApplied
        3 -> NUSStopped
        4 -> NUSStopping
        5 -> NUSWaitingToStart
        _ -> (error . showText) $ "Unknown index for NodeUpdateStatus: " <> toText i
    fromEnum x = case x of
        NUSComplete -> 0
        NUSInProgress -> 1
        NUSNotApplied -> 2
        NUSStopped -> 3
        NUSStopping -> 4
        NUSWaitingToStart -> 5
        NodeUpdateStatus' name -> (error . showText) $ "Unknown NodeUpdateStatus: " <> original name

-- | Represents the bounds of /known/ $NodeUpdateStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded NodeUpdateStatus where
    minBound = NUSComplete
    maxBound = NUSWaitingToStart

instance Hashable     NodeUpdateStatus
instance NFData       NodeUpdateStatus
instance ToByteString NodeUpdateStatus
instance ToQuery      NodeUpdateStatus
instance ToHeader     NodeUpdateStatus

instance FromXML NodeUpdateStatus where
    parseXML = parseXMLText "NodeUpdateStatus"
