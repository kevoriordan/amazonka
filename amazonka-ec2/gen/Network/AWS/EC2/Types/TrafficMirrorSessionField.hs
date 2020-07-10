{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TrafficMirrorSessionField
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TrafficMirrorSessionField (
  TrafficMirrorSessionField (
    ..
    , TMSFDescription
    , TMSFPacketLength
    , TMSFVirtualNetworkId
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TrafficMirrorSessionField = TrafficMirrorSessionField' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern TMSFDescription :: TrafficMirrorSessionField
pattern TMSFDescription = TrafficMirrorSessionField' "description"

pattern TMSFPacketLength :: TrafficMirrorSessionField
pattern TMSFPacketLength = TrafficMirrorSessionField' "packet-length"

pattern TMSFVirtualNetworkId :: TrafficMirrorSessionField
pattern TMSFVirtualNetworkId = TrafficMirrorSessionField' "virtual-network-id"

{-# COMPLETE
  TMSFDescription,
  TMSFPacketLength,
  TMSFVirtualNetworkId,
  TrafficMirrorSessionField' #-}

instance FromText TrafficMirrorSessionField where
    parser = (TrafficMirrorSessionField' . mk) <$> takeText

instance ToText TrafficMirrorSessionField where
    toText (TrafficMirrorSessionField' ci) = original ci

-- | Represents an enum of /known/ $TrafficMirrorSessionField.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TrafficMirrorSessionField where
    toEnum i = case i of
        0 -> TMSFDescription
        1 -> TMSFPacketLength
        2 -> TMSFVirtualNetworkId
        _ -> (error . showText) $ "Unknown index for TrafficMirrorSessionField: " <> toText i
    fromEnum x = case x of
        TMSFDescription -> 0
        TMSFPacketLength -> 1
        TMSFVirtualNetworkId -> 2
        TrafficMirrorSessionField' name -> (error . showText) $ "Unknown TrafficMirrorSessionField: " <> original name

-- | Represents the bounds of /known/ $TrafficMirrorSessionField.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TrafficMirrorSessionField where
    minBound = TMSFDescription
    maxBound = TMSFVirtualNetworkId

instance Hashable     TrafficMirrorSessionField
instance NFData       TrafficMirrorSessionField
instance ToByteString TrafficMirrorSessionField
instance ToQuery      TrafficMirrorSessionField
instance ToHeader     TrafficMirrorSessionField
