{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactGuidance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactGuidance (
  ProvisioningArtifactGuidance (
    ..
    , Default
    , Deprecated
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ProvisioningArtifactGuidance = ProvisioningArtifactGuidance' (CI
                                                                     Text)
                                      deriving (Eq, Ord, Read, Show, Data,
                                                Typeable, Generic)

pattern Default :: ProvisioningArtifactGuidance
pattern Default = ProvisioningArtifactGuidance' "DEFAULT"

pattern Deprecated :: ProvisioningArtifactGuidance
pattern Deprecated = ProvisioningArtifactGuidance' "DEPRECATED"

{-# COMPLETE
  Default,
  Deprecated,
  ProvisioningArtifactGuidance' #-}

instance FromText ProvisioningArtifactGuidance where
    parser = (ProvisioningArtifactGuidance' . mk) <$> takeText

instance ToText ProvisioningArtifactGuidance where
    toText (ProvisioningArtifactGuidance' ci) = original ci

-- | Represents an enum of /known/ $ProvisioningArtifactGuidance.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProvisioningArtifactGuidance where
    toEnum i = case i of
        0 -> Default
        1 -> Deprecated
        _ -> (error . showText) $ "Unknown index for ProvisioningArtifactGuidance: " <> toText i
    fromEnum x = case x of
        Default -> 0
        Deprecated -> 1
        ProvisioningArtifactGuidance' name -> (error . showText) $ "Unknown ProvisioningArtifactGuidance: " <> original name

-- | Represents the bounds of /known/ $ProvisioningArtifactGuidance.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProvisioningArtifactGuidance where
    minBound = Default
    maxBound = Deprecated

instance Hashable     ProvisioningArtifactGuidance
instance NFData       ProvisioningArtifactGuidance
instance ToByteString ProvisioningArtifactGuidance
instance ToQuery      ProvisioningArtifactGuidance
instance ToHeader     ProvisioningArtifactGuidance

instance ToJSON ProvisioningArtifactGuidance where
    toJSON = toJSONText

instance FromJSON ProvisioningArtifactGuidance where
    parseJSON = parseJSONText "ProvisioningArtifactGuidance"
