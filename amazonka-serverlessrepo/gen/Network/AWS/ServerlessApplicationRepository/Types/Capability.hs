{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.Capability
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServerlessApplicationRepository.Types.Capability (
  Capability (
    ..
    , CapabilityAutoExpand
    , CapabilityIAM
    , CapabilityNamedIAM
    , CapabilityResourcePolicy
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Values that must be specified in order to deploy some applications.
--
--
data Capability = Capability' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern CapabilityAutoExpand :: Capability
pattern CapabilityAutoExpand = Capability' "CAPABILITY_AUTO_EXPAND"

pattern CapabilityIAM :: Capability
pattern CapabilityIAM = Capability' "CAPABILITY_IAM"

pattern CapabilityNamedIAM :: Capability
pattern CapabilityNamedIAM = Capability' "CAPABILITY_NAMED_IAM"

pattern CapabilityResourcePolicy :: Capability
pattern CapabilityResourcePolicy = Capability' "CAPABILITY_RESOURCE_POLICY"

{-# COMPLETE
  CapabilityAutoExpand,
  CapabilityIAM,
  CapabilityNamedIAM,
  CapabilityResourcePolicy,
  Capability' #-}

instance FromText Capability where
    parser = (Capability' . mk) <$> takeText

instance ToText Capability where
    toText (Capability' ci) = original ci

-- | Represents an enum of /known/ $Capability.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Capability where
    toEnum i = case i of
        0 -> CapabilityAutoExpand
        1 -> CapabilityIAM
        2 -> CapabilityNamedIAM
        3 -> CapabilityResourcePolicy
        _ -> (error . showText) $ "Unknown index for Capability: " <> toText i
    fromEnum x = case x of
        CapabilityAutoExpand -> 0
        CapabilityIAM -> 1
        CapabilityNamedIAM -> 2
        CapabilityResourcePolicy -> 3
        Capability' name -> (error . showText) $ "Unknown Capability: " <> original name

-- | Represents the bounds of /known/ $Capability.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Capability where
    minBound = CapabilityAutoExpand
    maxBound = CapabilityResourcePolicy

instance Hashable     Capability
instance NFData       Capability
instance ToByteString Capability
instance ToQuery      Capability
instance ToHeader     Capability

instance FromJSON Capability where
    parseJSON = parseJSONText "Capability"
