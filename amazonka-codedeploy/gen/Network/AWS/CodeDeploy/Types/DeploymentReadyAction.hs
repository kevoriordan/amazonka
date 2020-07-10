{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentReadyAction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.DeploymentReadyAction (
  DeploymentReadyAction (
    ..
    , ContinueDeployment
    , StopDeployment
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DeploymentReadyAction = DeploymentReadyAction' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern ContinueDeployment :: DeploymentReadyAction
pattern ContinueDeployment = DeploymentReadyAction' "CONTINUE_DEPLOYMENT"

pattern StopDeployment :: DeploymentReadyAction
pattern StopDeployment = DeploymentReadyAction' "STOP_DEPLOYMENT"

{-# COMPLETE
  ContinueDeployment,
  StopDeployment,
  DeploymentReadyAction' #-}

instance FromText DeploymentReadyAction where
    parser = (DeploymentReadyAction' . mk) <$> takeText

instance ToText DeploymentReadyAction where
    toText (DeploymentReadyAction' ci) = original ci

-- | Represents an enum of /known/ $DeploymentReadyAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DeploymentReadyAction where
    toEnum i = case i of
        0 -> ContinueDeployment
        1 -> StopDeployment
        _ -> (error . showText) $ "Unknown index for DeploymentReadyAction: " <> toText i
    fromEnum x = case x of
        ContinueDeployment -> 0
        StopDeployment -> 1
        DeploymentReadyAction' name -> (error . showText) $ "Unknown DeploymentReadyAction: " <> original name

-- | Represents the bounds of /known/ $DeploymentReadyAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DeploymentReadyAction where
    minBound = ContinueDeployment
    maxBound = StopDeployment

instance Hashable     DeploymentReadyAction
instance NFData       DeploymentReadyAction
instance ToByteString DeploymentReadyAction
instance ToQuery      DeploymentReadyAction
instance ToHeader     DeploymentReadyAction

instance ToJSON DeploymentReadyAction where
    toJSON = toJSONText

instance FromJSON DeploymentReadyAction where
    parseJSON = parseJSONText "DeploymentReadyAction"
