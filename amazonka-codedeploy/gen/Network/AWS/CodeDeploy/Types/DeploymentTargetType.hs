{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentTargetType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.DeploymentTargetType (
  DeploymentTargetType (
    ..
    , ECSTarget
    , InstanceTarget
    , LambdaTarget
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DeploymentTargetType = DeploymentTargetType' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern ECSTarget :: DeploymentTargetType
pattern ECSTarget = DeploymentTargetType' "ECSTarget"

pattern InstanceTarget :: DeploymentTargetType
pattern InstanceTarget = DeploymentTargetType' "InstanceTarget"

pattern LambdaTarget :: DeploymentTargetType
pattern LambdaTarget = DeploymentTargetType' "LambdaTarget"

{-# COMPLETE
  ECSTarget,
  InstanceTarget,
  LambdaTarget,
  DeploymentTargetType' #-}

instance FromText DeploymentTargetType where
    parser = (DeploymentTargetType' . mk) <$> takeText

instance ToText DeploymentTargetType where
    toText (DeploymentTargetType' ci) = original ci

-- | Represents an enum of /known/ $DeploymentTargetType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DeploymentTargetType where
    toEnum i = case i of
        0 -> ECSTarget
        1 -> InstanceTarget
        2 -> LambdaTarget
        _ -> (error . showText) $ "Unknown index for DeploymentTargetType: " <> toText i
    fromEnum x = case x of
        ECSTarget -> 0
        InstanceTarget -> 1
        LambdaTarget -> 2
        DeploymentTargetType' name -> (error . showText) $ "Unknown DeploymentTargetType: " <> original name

-- | Represents the bounds of /known/ $DeploymentTargetType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DeploymentTargetType where
    minBound = ECSTarget
    maxBound = LambdaTarget

instance Hashable     DeploymentTargetType
instance NFData       DeploymentTargetType
instance ToByteString DeploymentTargetType
instance ToQuery      DeploymentTargetType
instance ToHeader     DeploymentTargetType

instance FromJSON DeploymentTargetType where
    parseJSON = parseJSONText "DeploymentTargetType"
