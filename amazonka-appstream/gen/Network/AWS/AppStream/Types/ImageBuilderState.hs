{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ImageBuilderState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.ImageBuilderState (
  ImageBuilderState (
    ..
    , IBSDeleting
    , IBSFailed
    , IBSPending
    , IBSRebooting
    , IBSRunning
    , IBSSnapshotting
    , IBSStopped
    , IBSStopping
    , IBSUpdatingAgent
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ImageBuilderState = ImageBuilderState' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern IBSDeleting :: ImageBuilderState
pattern IBSDeleting = ImageBuilderState' "DELETING"

pattern IBSFailed :: ImageBuilderState
pattern IBSFailed = ImageBuilderState' "FAILED"

pattern IBSPending :: ImageBuilderState
pattern IBSPending = ImageBuilderState' "PENDING"

pattern IBSRebooting :: ImageBuilderState
pattern IBSRebooting = ImageBuilderState' "REBOOTING"

pattern IBSRunning :: ImageBuilderState
pattern IBSRunning = ImageBuilderState' "RUNNING"

pattern IBSSnapshotting :: ImageBuilderState
pattern IBSSnapshotting = ImageBuilderState' "SNAPSHOTTING"

pattern IBSStopped :: ImageBuilderState
pattern IBSStopped = ImageBuilderState' "STOPPED"

pattern IBSStopping :: ImageBuilderState
pattern IBSStopping = ImageBuilderState' "STOPPING"

pattern IBSUpdatingAgent :: ImageBuilderState
pattern IBSUpdatingAgent = ImageBuilderState' "UPDATING_AGENT"

{-# COMPLETE
  IBSDeleting,
  IBSFailed,
  IBSPending,
  IBSRebooting,
  IBSRunning,
  IBSSnapshotting,
  IBSStopped,
  IBSStopping,
  IBSUpdatingAgent,
  ImageBuilderState' #-}

instance FromText ImageBuilderState where
    parser = (ImageBuilderState' . mk) <$> takeText

instance ToText ImageBuilderState where
    toText (ImageBuilderState' ci) = original ci

-- | Represents an enum of /known/ $ImageBuilderState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ImageBuilderState where
    toEnum i = case i of
        0 -> IBSDeleting
        1 -> IBSFailed
        2 -> IBSPending
        3 -> IBSRebooting
        4 -> IBSRunning
        5 -> IBSSnapshotting
        6 -> IBSStopped
        7 -> IBSStopping
        8 -> IBSUpdatingAgent
        _ -> (error . showText) $ "Unknown index for ImageBuilderState: " <> toText i
    fromEnum x = case x of
        IBSDeleting -> 0
        IBSFailed -> 1
        IBSPending -> 2
        IBSRebooting -> 3
        IBSRunning -> 4
        IBSSnapshotting -> 5
        IBSStopped -> 6
        IBSStopping -> 7
        IBSUpdatingAgent -> 8
        ImageBuilderState' name -> (error . showText) $ "Unknown ImageBuilderState: " <> original name

-- | Represents the bounds of /known/ $ImageBuilderState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ImageBuilderState where
    minBound = IBSDeleting
    maxBound = IBSUpdatingAgent

instance Hashable     ImageBuilderState
instance NFData       ImageBuilderState
instance ToByteString ImageBuilderState
instance ToQuery      ImageBuilderState
instance ToHeader     ImageBuilderState

instance FromJSON ImageBuilderState where
    parseJSON = parseJSONText "ImageBuilderState"
