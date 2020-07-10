{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.DeploymentCommandName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.DeploymentCommandName (
  DeploymentCommandName (
    ..
    , Configure
    , Deploy
    , ExecuteRecipes
    , InstallDependencies
    , Restart
    , Rollback
    , Setup
    , Start
    , Stop
    , Undeploy
    , UpdateCustomCookbooks
    , UpdateDependencies
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DeploymentCommandName = DeploymentCommandName' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern Configure :: DeploymentCommandName
pattern Configure = DeploymentCommandName' "configure"

pattern Deploy :: DeploymentCommandName
pattern Deploy = DeploymentCommandName' "deploy"

pattern ExecuteRecipes :: DeploymentCommandName
pattern ExecuteRecipes = DeploymentCommandName' "execute_recipes"

pattern InstallDependencies :: DeploymentCommandName
pattern InstallDependencies = DeploymentCommandName' "install_dependencies"

pattern Restart :: DeploymentCommandName
pattern Restart = DeploymentCommandName' "restart"

pattern Rollback :: DeploymentCommandName
pattern Rollback = DeploymentCommandName' "rollback"

pattern Setup :: DeploymentCommandName
pattern Setup = DeploymentCommandName' "setup"

pattern Start :: DeploymentCommandName
pattern Start = DeploymentCommandName' "start"

pattern Stop :: DeploymentCommandName
pattern Stop = DeploymentCommandName' "stop"

pattern Undeploy :: DeploymentCommandName
pattern Undeploy = DeploymentCommandName' "undeploy"

pattern UpdateCustomCookbooks :: DeploymentCommandName
pattern UpdateCustomCookbooks = DeploymentCommandName' "update_custom_cookbooks"

pattern UpdateDependencies :: DeploymentCommandName
pattern UpdateDependencies = DeploymentCommandName' "update_dependencies"

{-# COMPLETE
  Configure,
  Deploy,
  ExecuteRecipes,
  InstallDependencies,
  Restart,
  Rollback,
  Setup,
  Start,
  Stop,
  Undeploy,
  UpdateCustomCookbooks,
  UpdateDependencies,
  DeploymentCommandName' #-}

instance FromText DeploymentCommandName where
    parser = (DeploymentCommandName' . mk) <$> takeText

instance ToText DeploymentCommandName where
    toText (DeploymentCommandName' ci) = original ci

-- | Represents an enum of /known/ $DeploymentCommandName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DeploymentCommandName where
    toEnum i = case i of
        0 -> Configure
        1 -> Deploy
        2 -> ExecuteRecipes
        3 -> InstallDependencies
        4 -> Restart
        5 -> Rollback
        6 -> Setup
        7 -> Start
        8 -> Stop
        9 -> Undeploy
        10 -> UpdateCustomCookbooks
        11 -> UpdateDependencies
        _ -> (error . showText) $ "Unknown index for DeploymentCommandName: " <> toText i
    fromEnum x = case x of
        Configure -> 0
        Deploy -> 1
        ExecuteRecipes -> 2
        InstallDependencies -> 3
        Restart -> 4
        Rollback -> 5
        Setup -> 6
        Start -> 7
        Stop -> 8
        Undeploy -> 9
        UpdateCustomCookbooks -> 10
        UpdateDependencies -> 11
        DeploymentCommandName' name -> (error . showText) $ "Unknown DeploymentCommandName: " <> original name

-- | Represents the bounds of /known/ $DeploymentCommandName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DeploymentCommandName where
    minBound = Configure
    maxBound = UpdateDependencies

instance Hashable     DeploymentCommandName
instance NFData       DeploymentCommandName
instance ToByteString DeploymentCommandName
instance ToQuery      DeploymentCommandName
instance ToHeader     DeploymentCommandName

instance ToJSON DeploymentCommandName where
    toJSON = toJSONText

instance FromJSON DeploymentCommandName where
    parseJSON = parseJSONText "DeploymentCommandName"
