{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SnapshotState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.SnapshotState (
  SnapshotState (
    ..
    , SSCompleted
    , SSError'
    , SSPending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data SnapshotState = SnapshotState' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern SSCompleted :: SnapshotState
pattern SSCompleted = SnapshotState' "completed"

pattern SSError' :: SnapshotState
pattern SSError' = SnapshotState' "error"

pattern SSPending :: SnapshotState
pattern SSPending = SnapshotState' "pending"

{-# COMPLETE
  SSCompleted,
  SSError',
  SSPending,
  SnapshotState' #-}

instance FromText SnapshotState where
    parser = (SnapshotState' . mk) <$> takeText

instance ToText SnapshotState where
    toText (SnapshotState' ci) = original ci

-- | Represents an enum of /known/ $SnapshotState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SnapshotState where
    toEnum i = case i of
        0 -> SSCompleted
        1 -> SSError'
        2 -> SSPending
        _ -> (error . showText) $ "Unknown index for SnapshotState: " <> toText i
    fromEnum x = case x of
        SSCompleted -> 0
        SSError' -> 1
        SSPending -> 2
        SnapshotState' name -> (error . showText) $ "Unknown SnapshotState: " <> original name

-- | Represents the bounds of /known/ $SnapshotState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SnapshotState where
    minBound = SSCompleted
    maxBound = SSPending

instance Hashable     SnapshotState
instance NFData       SnapshotState
instance ToByteString SnapshotState
instance ToQuery      SnapshotState
instance ToHeader     SnapshotState

instance FromXML SnapshotState where
    parseXML = parseXMLText "SnapshotState"
