{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentCreator
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.DeploymentCreator (
  DeploymentCreator (
    ..
    , Autoscaling
    , CodeDeployRollback
    , User
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DeploymentCreator = DeploymentCreator' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern Autoscaling :: DeploymentCreator
pattern Autoscaling = DeploymentCreator' "autoscaling"

pattern CodeDeployRollback :: DeploymentCreator
pattern CodeDeployRollback = DeploymentCreator' "codeDeployRollback"

pattern User :: DeploymentCreator
pattern User = DeploymentCreator' "user"

{-# COMPLETE
  Autoscaling,
  CodeDeployRollback,
  User,
  DeploymentCreator' #-}

instance FromText DeploymentCreator where
    parser = (DeploymentCreator' . mk) <$> takeText

instance ToText DeploymentCreator where
    toText (DeploymentCreator' ci) = original ci

-- | Represents an enum of /known/ $DeploymentCreator.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DeploymentCreator where
    toEnum i = case i of
        0 -> Autoscaling
        1 -> CodeDeployRollback
        2 -> User
        _ -> (error . showText) $ "Unknown index for DeploymentCreator: " <> toText i
    fromEnum x = case x of
        Autoscaling -> 0
        CodeDeployRollback -> 1
        User -> 2
        DeploymentCreator' name -> (error . showText) $ "Unknown DeploymentCreator: " <> original name

-- | Represents the bounds of /known/ $DeploymentCreator.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DeploymentCreator where
    minBound = Autoscaling
    maxBound = User

instance Hashable     DeploymentCreator
instance NFData       DeploymentCreator
instance ToByteString DeploymentCreator
instance ToQuery      DeploymentCreator
instance ToHeader     DeploymentCreator

instance FromJSON DeploymentCreator where
    parseJSON = parseJSONText "DeploymentCreator"
