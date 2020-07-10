{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.AgentUpdateStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.AgentUpdateStatus (
  AgentUpdateStatus (
    ..
    , AUSFailed
    , AUSPending
    , AUSStaged
    , AUSStaging
    , AUSUpdated
    , AUSUpdating
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AgentUpdateStatus = AgentUpdateStatus' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern AUSFailed :: AgentUpdateStatus
pattern AUSFailed = AgentUpdateStatus' "FAILED"

pattern AUSPending :: AgentUpdateStatus
pattern AUSPending = AgentUpdateStatus' "PENDING"

pattern AUSStaged :: AgentUpdateStatus
pattern AUSStaged = AgentUpdateStatus' "STAGED"

pattern AUSStaging :: AgentUpdateStatus
pattern AUSStaging = AgentUpdateStatus' "STAGING"

pattern AUSUpdated :: AgentUpdateStatus
pattern AUSUpdated = AgentUpdateStatus' "UPDATED"

pattern AUSUpdating :: AgentUpdateStatus
pattern AUSUpdating = AgentUpdateStatus' "UPDATING"

{-# COMPLETE
  AUSFailed,
  AUSPending,
  AUSStaged,
  AUSStaging,
  AUSUpdated,
  AUSUpdating,
  AgentUpdateStatus' #-}

instance FromText AgentUpdateStatus where
    parser = (AgentUpdateStatus' . mk) <$> takeText

instance ToText AgentUpdateStatus where
    toText (AgentUpdateStatus' ci) = original ci

-- | Represents an enum of /known/ $AgentUpdateStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AgentUpdateStatus where
    toEnum i = case i of
        0 -> AUSFailed
        1 -> AUSPending
        2 -> AUSStaged
        3 -> AUSStaging
        4 -> AUSUpdated
        5 -> AUSUpdating
        _ -> (error . showText) $ "Unknown index for AgentUpdateStatus: " <> toText i
    fromEnum x = case x of
        AUSFailed -> 0
        AUSPending -> 1
        AUSStaged -> 2
        AUSStaging -> 3
        AUSUpdated -> 4
        AUSUpdating -> 5
        AgentUpdateStatus' name -> (error . showText) $ "Unknown AgentUpdateStatus: " <> original name

-- | Represents the bounds of /known/ $AgentUpdateStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AgentUpdateStatus where
    minBound = AUSFailed
    maxBound = AUSUpdating

instance Hashable     AgentUpdateStatus
instance NFData       AgentUpdateStatus
instance ToByteString AgentUpdateStatus
instance ToQuery      AgentUpdateStatus
instance ToHeader     AgentUpdateStatus

instance FromJSON AgentUpdateStatus where
    parseJSON = parseJSONText "AgentUpdateStatus"
