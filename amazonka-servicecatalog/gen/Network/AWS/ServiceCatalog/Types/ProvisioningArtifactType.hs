{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactType (
  ProvisioningArtifactType (
    ..
    , PATCloudFormationTemplate
    , PATMarketplaceAMI
    , PATMarketplaceCar
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ProvisioningArtifactType = ProvisioningArtifactType' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern PATCloudFormationTemplate :: ProvisioningArtifactType
pattern PATCloudFormationTemplate = ProvisioningArtifactType' "CLOUD_FORMATION_TEMPLATE"

pattern PATMarketplaceAMI :: ProvisioningArtifactType
pattern PATMarketplaceAMI = ProvisioningArtifactType' "MARKETPLACE_AMI"

pattern PATMarketplaceCar :: ProvisioningArtifactType
pattern PATMarketplaceCar = ProvisioningArtifactType' "MARKETPLACE_CAR"

{-# COMPLETE
  PATCloudFormationTemplate,
  PATMarketplaceAMI,
  PATMarketplaceCar,
  ProvisioningArtifactType' #-}

instance FromText ProvisioningArtifactType where
    parser = (ProvisioningArtifactType' . mk) <$> takeText

instance ToText ProvisioningArtifactType where
    toText (ProvisioningArtifactType' ci) = original ci

-- | Represents an enum of /known/ $ProvisioningArtifactType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProvisioningArtifactType where
    toEnum i = case i of
        0 -> PATCloudFormationTemplate
        1 -> PATMarketplaceAMI
        2 -> PATMarketplaceCar
        _ -> (error . showText) $ "Unknown index for ProvisioningArtifactType: " <> toText i
    fromEnum x = case x of
        PATCloudFormationTemplate -> 0
        PATMarketplaceAMI -> 1
        PATMarketplaceCar -> 2
        ProvisioningArtifactType' name -> (error . showText) $ "Unknown ProvisioningArtifactType: " <> original name

-- | Represents the bounds of /known/ $ProvisioningArtifactType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProvisioningArtifactType where
    minBound = PATCloudFormationTemplate
    maxBound = PATMarketplaceCar

instance Hashable     ProvisioningArtifactType
instance NFData       ProvisioningArtifactType
instance ToByteString ProvisioningArtifactType
instance ToQuery      ProvisioningArtifactType
instance ToHeader     ProvisioningArtifactType

instance ToJSON ProvisioningArtifactType where
    toJSON = toJSONText

instance FromJSON ProvisioningArtifactType where
    parseJSON = parseJSONText "ProvisioningArtifactType"
