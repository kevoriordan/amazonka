{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.DeploymentType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Greengrass.Types.DeploymentType (
  DeploymentType (
    ..
    , ForceResetDeployment
    , NewDeployment
    , Redeployment
    , ResetDeployment
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The type of deployment. When used for ''CreateDeployment'', only ''NewDeployment'' and ''Redeployment'' are valid.
data DeploymentType = DeploymentType' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern ForceResetDeployment :: DeploymentType
pattern ForceResetDeployment = DeploymentType' "ForceResetDeployment"

pattern NewDeployment :: DeploymentType
pattern NewDeployment = DeploymentType' "NewDeployment"

pattern Redeployment :: DeploymentType
pattern Redeployment = DeploymentType' "Redeployment"

pattern ResetDeployment :: DeploymentType
pattern ResetDeployment = DeploymentType' "ResetDeployment"

{-# COMPLETE
  ForceResetDeployment,
  NewDeployment,
  Redeployment,
  ResetDeployment,
  DeploymentType' #-}

instance FromText DeploymentType where
    parser = (DeploymentType' . mk) <$> takeText

instance ToText DeploymentType where
    toText (DeploymentType' ci) = original ci

-- | Represents an enum of /known/ $DeploymentType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DeploymentType where
    toEnum i = case i of
        0 -> ForceResetDeployment
        1 -> NewDeployment
        2 -> Redeployment
        3 -> ResetDeployment
        _ -> (error . showText) $ "Unknown index for DeploymentType: " <> toText i
    fromEnum x = case x of
        ForceResetDeployment -> 0
        NewDeployment -> 1
        Redeployment -> 2
        ResetDeployment -> 3
        DeploymentType' name -> (error . showText) $ "Unknown DeploymentType: " <> original name

-- | Represents the bounds of /known/ $DeploymentType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DeploymentType where
    minBound = ForceResetDeployment
    maxBound = ResetDeployment

instance Hashable     DeploymentType
instance NFData       DeploymentType
instance ToByteString DeploymentType
instance ToQuery      DeploymentType
instance ToHeader     DeploymentType

instance ToJSON DeploymentType where
    toJSON = toJSONText

instance FromJSON DeploymentType where
    parseJSON = parseJSONText "DeploymentType"
