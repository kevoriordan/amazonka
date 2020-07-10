{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.AutoRollbackEvent
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.AutoRollbackEvent (
  AutoRollbackEvent (
    ..
    , AREDeploymentFailure
    , AREDeploymentStopOnAlarm
    , AREDeploymentStopOnRequest
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AutoRollbackEvent = AutoRollbackEvent' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern AREDeploymentFailure :: AutoRollbackEvent
pattern AREDeploymentFailure = AutoRollbackEvent' "DEPLOYMENT_FAILURE"

pattern AREDeploymentStopOnAlarm :: AutoRollbackEvent
pattern AREDeploymentStopOnAlarm = AutoRollbackEvent' "DEPLOYMENT_STOP_ON_ALARM"

pattern AREDeploymentStopOnRequest :: AutoRollbackEvent
pattern AREDeploymentStopOnRequest = AutoRollbackEvent' "DEPLOYMENT_STOP_ON_REQUEST"

{-# COMPLETE
  AREDeploymentFailure,
  AREDeploymentStopOnAlarm,
  AREDeploymentStopOnRequest,
  AutoRollbackEvent' #-}

instance FromText AutoRollbackEvent where
    parser = (AutoRollbackEvent' . mk) <$> takeText

instance ToText AutoRollbackEvent where
    toText (AutoRollbackEvent' ci) = original ci

-- | Represents an enum of /known/ $AutoRollbackEvent.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AutoRollbackEvent where
    toEnum i = case i of
        0 -> AREDeploymentFailure
        1 -> AREDeploymentStopOnAlarm
        2 -> AREDeploymentStopOnRequest
        _ -> (error . showText) $ "Unknown index for AutoRollbackEvent: " <> toText i
    fromEnum x = case x of
        AREDeploymentFailure -> 0
        AREDeploymentStopOnAlarm -> 1
        AREDeploymentStopOnRequest -> 2
        AutoRollbackEvent' name -> (error . showText) $ "Unknown AutoRollbackEvent: " <> original name

-- | Represents the bounds of /known/ $AutoRollbackEvent.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AutoRollbackEvent where
    minBound = AREDeploymentFailure
    maxBound = AREDeploymentStopOnRequest

instance Hashable     AutoRollbackEvent
instance NFData       AutoRollbackEvent
instance ToByteString AutoRollbackEvent
instance ToQuery      AutoRollbackEvent
instance ToHeader     AutoRollbackEvent

instance ToJSON AutoRollbackEvent where
    toJSON = toJSONText

instance FromJSON AutoRollbackEvent where
    parseJSON = parseJSONText "AutoRollbackEvent"
