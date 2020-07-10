{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceSnapshotState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.InstanceSnapshotState (
  InstanceSnapshotState (
    ..
    , ISSAvailable
    , ISSError'
    , ISSPending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data InstanceSnapshotState = InstanceSnapshotState' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern ISSAvailable :: InstanceSnapshotState
pattern ISSAvailable = InstanceSnapshotState' "available"

pattern ISSError' :: InstanceSnapshotState
pattern ISSError' = InstanceSnapshotState' "error"

pattern ISSPending :: InstanceSnapshotState
pattern ISSPending = InstanceSnapshotState' "pending"

{-# COMPLETE
  ISSAvailable,
  ISSError',
  ISSPending,
  InstanceSnapshotState' #-}

instance FromText InstanceSnapshotState where
    parser = (InstanceSnapshotState' . mk) <$> takeText

instance ToText InstanceSnapshotState where
    toText (InstanceSnapshotState' ci) = original ci

-- | Represents an enum of /known/ $InstanceSnapshotState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InstanceSnapshotState where
    toEnum i = case i of
        0 -> ISSAvailable
        1 -> ISSError'
        2 -> ISSPending
        _ -> (error . showText) $ "Unknown index for InstanceSnapshotState: " <> toText i
    fromEnum x = case x of
        ISSAvailable -> 0
        ISSError' -> 1
        ISSPending -> 2
        InstanceSnapshotState' name -> (error . showText) $ "Unknown InstanceSnapshotState: " <> original name

-- | Represents the bounds of /known/ $InstanceSnapshotState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InstanceSnapshotState where
    minBound = ISSAvailable
    maxBound = ISSPending

instance Hashable     InstanceSnapshotState
instance NFData       InstanceSnapshotState
instance ToByteString InstanceSnapshotState
instance ToQuery      InstanceSnapshotState
instance ToHeader     InstanceSnapshotState

instance FromJSON InstanceSnapshotState where
    parseJSON = parseJSONText "InstanceSnapshotState"
