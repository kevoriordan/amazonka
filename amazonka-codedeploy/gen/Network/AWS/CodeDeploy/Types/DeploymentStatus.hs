{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.DeploymentStatus (
  DeploymentStatus (
    ..
    , Created
    , Failed
    , InProgress
    , Queued
    , Ready
    , Stopped
    , Succeeded
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DeploymentStatus = DeploymentStatus' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern Created :: DeploymentStatus
pattern Created = DeploymentStatus' "Created"

pattern Failed :: DeploymentStatus
pattern Failed = DeploymentStatus' "Failed"

pattern InProgress :: DeploymentStatus
pattern InProgress = DeploymentStatus' "InProgress"

pattern Queued :: DeploymentStatus
pattern Queued = DeploymentStatus' "Queued"

pattern Ready :: DeploymentStatus
pattern Ready = DeploymentStatus' "Ready"

pattern Stopped :: DeploymentStatus
pattern Stopped = DeploymentStatus' "Stopped"

pattern Succeeded :: DeploymentStatus
pattern Succeeded = DeploymentStatus' "Succeeded"

{-# COMPLETE
  Created,
  Failed,
  InProgress,
  Queued,
  Ready,
  Stopped,
  Succeeded,
  DeploymentStatus' #-}

instance FromText DeploymentStatus where
    parser = (DeploymentStatus' . mk) <$> takeText

instance ToText DeploymentStatus where
    toText (DeploymentStatus' ci) = original ci

-- | Represents an enum of /known/ $DeploymentStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DeploymentStatus where
    toEnum i = case i of
        0 -> Created
        1 -> Failed
        2 -> InProgress
        3 -> Queued
        4 -> Ready
        5 -> Stopped
        6 -> Succeeded
        _ -> (error . showText) $ "Unknown index for DeploymentStatus: " <> toText i
    fromEnum x = case x of
        Created -> 0
        Failed -> 1
        InProgress -> 2
        Queued -> 3
        Ready -> 4
        Stopped -> 5
        Succeeded -> 6
        DeploymentStatus' name -> (error . showText) $ "Unknown DeploymentStatus: " <> original name

-- | Represents the bounds of /known/ $DeploymentStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DeploymentStatus where
    minBound = Created
    maxBound = Succeeded

instance Hashable     DeploymentStatus
instance NFData       DeploymentStatus
instance ToByteString DeploymentStatus
instance ToQuery      DeploymentStatus
instance ToHeader     DeploymentStatus

instance ToJSON DeploymentStatus where
    toJSON = toJSONText

instance FromJSON DeploymentStatus where
    parseJSON = parseJSONText "DeploymentStatus"
