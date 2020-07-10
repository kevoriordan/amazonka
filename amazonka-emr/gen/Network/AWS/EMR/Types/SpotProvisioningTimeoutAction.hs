{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.SpotProvisioningTimeoutAction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.SpotProvisioningTimeoutAction (
  SpotProvisioningTimeoutAction (
    ..
    , SPTASwitchToOnDemand
    , SPTATerminateCluster
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SpotProvisioningTimeoutAction = SpotProvisioningTimeoutAction' (CI
                                                                       Text)
                                       deriving (Eq, Ord, Read, Show, Data,
                                                 Typeable, Generic)

pattern SPTASwitchToOnDemand :: SpotProvisioningTimeoutAction
pattern SPTASwitchToOnDemand = SpotProvisioningTimeoutAction' "SWITCH_TO_ON_DEMAND"

pattern SPTATerminateCluster :: SpotProvisioningTimeoutAction
pattern SPTATerminateCluster = SpotProvisioningTimeoutAction' "TERMINATE_CLUSTER"

{-# COMPLETE
  SPTASwitchToOnDemand,
  SPTATerminateCluster,
  SpotProvisioningTimeoutAction' #-}

instance FromText SpotProvisioningTimeoutAction where
    parser = (SpotProvisioningTimeoutAction' . mk) <$> takeText

instance ToText SpotProvisioningTimeoutAction where
    toText (SpotProvisioningTimeoutAction' ci) = original ci

-- | Represents an enum of /known/ $SpotProvisioningTimeoutAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SpotProvisioningTimeoutAction where
    toEnum i = case i of
        0 -> SPTASwitchToOnDemand
        1 -> SPTATerminateCluster
        _ -> (error . showText) $ "Unknown index for SpotProvisioningTimeoutAction: " <> toText i
    fromEnum x = case x of
        SPTASwitchToOnDemand -> 0
        SPTATerminateCluster -> 1
        SpotProvisioningTimeoutAction' name -> (error . showText) $ "Unknown SpotProvisioningTimeoutAction: " <> original name

-- | Represents the bounds of /known/ $SpotProvisioningTimeoutAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SpotProvisioningTimeoutAction where
    minBound = SPTASwitchToOnDemand
    maxBound = SPTATerminateCluster

instance Hashable     SpotProvisioningTimeoutAction
instance NFData       SpotProvisioningTimeoutAction
instance ToByteString SpotProvisioningTimeoutAction
instance ToQuery      SpotProvisioningTimeoutAction
instance ToHeader     SpotProvisioningTimeoutAction

instance ToJSON SpotProvisioningTimeoutAction where
    toJSON = toJSONText

instance FromJSON SpotProvisioningTimeoutAction where
    parseJSON = parseJSONText "SpotProvisioningTimeoutAction"
