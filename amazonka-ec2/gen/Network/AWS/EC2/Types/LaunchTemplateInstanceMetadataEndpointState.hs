{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataEndpointState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataEndpointState (
  LaunchTemplateInstanceMetadataEndpointState (
    ..
    , LTIMESDisabled
    , LTIMESEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data LaunchTemplateInstanceMetadataEndpointState = LaunchTemplateInstanceMetadataEndpointState' (CI
                                                                                                   Text)
                                                     deriving (Eq, Ord, Read,
                                                               Show, Data,
                                                               Typeable,
                                                               Generic)

pattern LTIMESDisabled :: LaunchTemplateInstanceMetadataEndpointState
pattern LTIMESDisabled = LaunchTemplateInstanceMetadataEndpointState' "disabled"

pattern LTIMESEnabled :: LaunchTemplateInstanceMetadataEndpointState
pattern LTIMESEnabled = LaunchTemplateInstanceMetadataEndpointState' "enabled"

{-# COMPLETE
  LTIMESDisabled,
  LTIMESEnabled,
  LaunchTemplateInstanceMetadataEndpointState' #-}

instance FromText LaunchTemplateInstanceMetadataEndpointState where
    parser = (LaunchTemplateInstanceMetadataEndpointState' . mk) <$> takeText

instance ToText LaunchTemplateInstanceMetadataEndpointState where
    toText (LaunchTemplateInstanceMetadataEndpointState' ci) = original ci

-- | Represents an enum of /known/ $LaunchTemplateInstanceMetadataEndpointState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LaunchTemplateInstanceMetadataEndpointState where
    toEnum i = case i of
        0 -> LTIMESDisabled
        1 -> LTIMESEnabled
        _ -> (error . showText) $ "Unknown index for LaunchTemplateInstanceMetadataEndpointState: " <> toText i
    fromEnum x = case x of
        LTIMESDisabled -> 0
        LTIMESEnabled -> 1
        LaunchTemplateInstanceMetadataEndpointState' name -> (error . showText) $ "Unknown LaunchTemplateInstanceMetadataEndpointState: " <> original name

-- | Represents the bounds of /known/ $LaunchTemplateInstanceMetadataEndpointState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LaunchTemplateInstanceMetadataEndpointState where
    minBound = LTIMESDisabled
    maxBound = LTIMESEnabled

instance Hashable     LaunchTemplateInstanceMetadataEndpointState
instance NFData       LaunchTemplateInstanceMetadataEndpointState
instance ToByteString LaunchTemplateInstanceMetadataEndpointState
instance ToQuery      LaunchTemplateInstanceMetadataEndpointState
instance ToHeader     LaunchTemplateInstanceMetadataEndpointState

instance FromXML LaunchTemplateInstanceMetadataEndpointState where
    parseXML = parseXMLText "LaunchTemplateInstanceMetadataEndpointState"
