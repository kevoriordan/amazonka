{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.SnapshotStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.SnapshotStatus (
  SnapshotStatus (
    ..
    , SSCompleted
    , SSCreating
    , SSFailed
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SnapshotStatus = SnapshotStatus' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern SSCompleted :: SnapshotStatus
pattern SSCompleted = SnapshotStatus' "Completed"

pattern SSCreating :: SnapshotStatus
pattern SSCreating = SnapshotStatus' "Creating"

pattern SSFailed :: SnapshotStatus
pattern SSFailed = SnapshotStatus' "Failed"

{-# COMPLETE
  SSCompleted,
  SSCreating,
  SSFailed,
  SnapshotStatus' #-}

instance FromText SnapshotStatus where
    parser = (SnapshotStatus' . mk) <$> takeText

instance ToText SnapshotStatus where
    toText (SnapshotStatus' ci) = original ci

-- | Represents an enum of /known/ $SnapshotStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SnapshotStatus where
    toEnum i = case i of
        0 -> SSCompleted
        1 -> SSCreating
        2 -> SSFailed
        _ -> (error . showText) $ "Unknown index for SnapshotStatus: " <> toText i
    fromEnum x = case x of
        SSCompleted -> 0
        SSCreating -> 1
        SSFailed -> 2
        SnapshotStatus' name -> (error . showText) $ "Unknown SnapshotStatus: " <> original name

-- | Represents the bounds of /known/ $SnapshotStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SnapshotStatus where
    minBound = SSCompleted
    maxBound = SSFailed

instance Hashable     SnapshotStatus
instance NFData       SnapshotStatus
instance ToByteString SnapshotStatus
instance ToQuery      SnapshotStatus
instance ToHeader     SnapshotStatus

instance FromJSON SnapshotStatus where
    parseJSON = parseJSONText "SnapshotStatus"
