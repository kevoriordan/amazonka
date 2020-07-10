{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceMetadataEndpointState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InstanceMetadataEndpointState (
  InstanceMetadataEndpointState (
    ..
    , IMESDisabled
    , IMESEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data InstanceMetadataEndpointState = InstanceMetadataEndpointState' (CI
                                                                       Text)
                                       deriving (Eq, Ord, Read, Show, Data,
                                                 Typeable, Generic)

pattern IMESDisabled :: InstanceMetadataEndpointState
pattern IMESDisabled = InstanceMetadataEndpointState' "disabled"

pattern IMESEnabled :: InstanceMetadataEndpointState
pattern IMESEnabled = InstanceMetadataEndpointState' "enabled"

{-# COMPLETE
  IMESDisabled,
  IMESEnabled,
  InstanceMetadataEndpointState' #-}

instance FromText InstanceMetadataEndpointState where
    parser = (InstanceMetadataEndpointState' . mk) <$> takeText

instance ToText InstanceMetadataEndpointState where
    toText (InstanceMetadataEndpointState' ci) = original ci

-- | Represents an enum of /known/ $InstanceMetadataEndpointState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InstanceMetadataEndpointState where
    toEnum i = case i of
        0 -> IMESDisabled
        1 -> IMESEnabled
        _ -> (error . showText) $ "Unknown index for InstanceMetadataEndpointState: " <> toText i
    fromEnum x = case x of
        IMESDisabled -> 0
        IMESEnabled -> 1
        InstanceMetadataEndpointState' name -> (error . showText) $ "Unknown InstanceMetadataEndpointState: " <> original name

-- | Represents the bounds of /known/ $InstanceMetadataEndpointState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InstanceMetadataEndpointState where
    minBound = IMESDisabled
    maxBound = IMESEnabled

instance Hashable     InstanceMetadataEndpointState
instance NFData       InstanceMetadataEndpointState
instance ToByteString InstanceMetadataEndpointState
instance ToQuery      InstanceMetadataEndpointState
instance ToHeader     InstanceMetadataEndpointState

instance FromXML InstanceMetadataEndpointState where
    parseXML = parseXMLText "InstanceMetadataEndpointState"
