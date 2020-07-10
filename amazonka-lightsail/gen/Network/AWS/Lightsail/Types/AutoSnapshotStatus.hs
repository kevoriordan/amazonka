{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.AutoSnapshotStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.AutoSnapshotStatus (
  AutoSnapshotStatus (
    ..
    , Failed
    , InProgress
    , NotFound
    , Success
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AutoSnapshotStatus = AutoSnapshotStatus' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern Failed :: AutoSnapshotStatus
pattern Failed = AutoSnapshotStatus' "Failed"

pattern InProgress :: AutoSnapshotStatus
pattern InProgress = AutoSnapshotStatus' "InProgress"

pattern NotFound :: AutoSnapshotStatus
pattern NotFound = AutoSnapshotStatus' "NotFound"

pattern Success :: AutoSnapshotStatus
pattern Success = AutoSnapshotStatus' "Success"

{-# COMPLETE
  Failed,
  InProgress,
  NotFound,
  Success,
  AutoSnapshotStatus' #-}

instance FromText AutoSnapshotStatus where
    parser = (AutoSnapshotStatus' . mk) <$> takeText

instance ToText AutoSnapshotStatus where
    toText (AutoSnapshotStatus' ci) = original ci

-- | Represents an enum of /known/ $AutoSnapshotStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AutoSnapshotStatus where
    toEnum i = case i of
        0 -> Failed
        1 -> InProgress
        2 -> NotFound
        3 -> Success
        _ -> (error . showText) $ "Unknown index for AutoSnapshotStatus: " <> toText i
    fromEnum x = case x of
        Failed -> 0
        InProgress -> 1
        NotFound -> 2
        Success -> 3
        AutoSnapshotStatus' name -> (error . showText) $ "Unknown AutoSnapshotStatus: " <> original name

-- | Represents the bounds of /known/ $AutoSnapshotStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AutoSnapshotStatus where
    minBound = Failed
    maxBound = Success

instance Hashable     AutoSnapshotStatus
instance NFData       AutoSnapshotStatus
instance ToByteString AutoSnapshotStatus
instance ToQuery      AutoSnapshotStatus
instance ToHeader     AutoSnapshotStatus

instance FromJSON AutoSnapshotStatus where
    parseJSON = parseJSONText "AutoSnapshotStatus"
