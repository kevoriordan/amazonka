{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactPropertyName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactPropertyName (
  ProvisioningArtifactPropertyName (
    ..
    , Id
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ProvisioningArtifactPropertyName = ProvisioningArtifactPropertyName' (CI
                                                                             Text)
                                          deriving (Eq, Ord, Read, Show, Data,
                                                    Typeable, Generic)

pattern Id :: ProvisioningArtifactPropertyName
pattern Id = ProvisioningArtifactPropertyName' "Id"

{-# COMPLETE
  Id,
  ProvisioningArtifactPropertyName' #-}

instance FromText ProvisioningArtifactPropertyName where
    parser = (ProvisioningArtifactPropertyName' . mk) <$> takeText

instance ToText ProvisioningArtifactPropertyName where
    toText (ProvisioningArtifactPropertyName' ci) = original ci

-- | Represents an enum of /known/ $ProvisioningArtifactPropertyName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProvisioningArtifactPropertyName where
    toEnum i = case i of
        0 -> Id
        _ -> (error . showText) $ "Unknown index for ProvisioningArtifactPropertyName: " <> toText i
    fromEnum x = case x of
        Id -> 0
        ProvisioningArtifactPropertyName' name -> (error . showText) $ "Unknown ProvisioningArtifactPropertyName: " <> original name

-- | Represents the bounds of /known/ $ProvisioningArtifactPropertyName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProvisioningArtifactPropertyName where
    minBound = Id
    maxBound = Id

instance Hashable     ProvisioningArtifactPropertyName
instance NFData       ProvisioningArtifactPropertyName
instance ToByteString ProvisioningArtifactPropertyName
instance ToQuery      ProvisioningArtifactPropertyName
instance ToHeader     ProvisioningArtifactPropertyName

instance ToJSON ProvisioningArtifactPropertyName where
    toJSON = toJSONText
