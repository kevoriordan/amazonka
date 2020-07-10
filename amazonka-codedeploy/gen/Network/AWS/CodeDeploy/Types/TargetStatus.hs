{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TargetStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.TargetStatus (
  TargetStatus (
    ..
    , TSFailed
    , TSInProgress
    , TSPending
    , TSReady
    , TSSkipped
    , TSSucceeded
    , TSUnknown
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TargetStatus = TargetStatus' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern TSFailed :: TargetStatus
pattern TSFailed = TargetStatus' "Failed"

pattern TSInProgress :: TargetStatus
pattern TSInProgress = TargetStatus' "InProgress"

pattern TSPending :: TargetStatus
pattern TSPending = TargetStatus' "Pending"

pattern TSReady :: TargetStatus
pattern TSReady = TargetStatus' "Ready"

pattern TSSkipped :: TargetStatus
pattern TSSkipped = TargetStatus' "Skipped"

pattern TSSucceeded :: TargetStatus
pattern TSSucceeded = TargetStatus' "Succeeded"

pattern TSUnknown :: TargetStatus
pattern TSUnknown = TargetStatus' "Unknown"

{-# COMPLETE
  TSFailed,
  TSInProgress,
  TSPending,
  TSReady,
  TSSkipped,
  TSSucceeded,
  TSUnknown,
  TargetStatus' #-}

instance FromText TargetStatus where
    parser = (TargetStatus' . mk) <$> takeText

instance ToText TargetStatus where
    toText (TargetStatus' ci) = original ci

-- | Represents an enum of /known/ $TargetStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TargetStatus where
    toEnum i = case i of
        0 -> TSFailed
        1 -> TSInProgress
        2 -> TSPending
        3 -> TSReady
        4 -> TSSkipped
        5 -> TSSucceeded
        6 -> TSUnknown
        _ -> (error . showText) $ "Unknown index for TargetStatus: " <> toText i
    fromEnum x = case x of
        TSFailed -> 0
        TSInProgress -> 1
        TSPending -> 2
        TSReady -> 3
        TSSkipped -> 4
        TSSucceeded -> 5
        TSUnknown -> 6
        TargetStatus' name -> (error . showText) $ "Unknown TargetStatus: " <> original name

-- | Represents the bounds of /known/ $TargetStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TargetStatus where
    minBound = TSFailed
    maxBound = TSUnknown

instance Hashable     TargetStatus
instance NFData       TargetStatus
instance ToByteString TargetStatus
instance ToQuery      TargetStatus
instance ToHeader     TargetStatus

instance FromJSON TargetStatus where
    parseJSON = parseJSONText "TargetStatus"
