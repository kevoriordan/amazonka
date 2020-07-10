{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPCEndpointType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VPCEndpointType (
  VPCEndpointType (
    ..
    , VETGateway
    , VETInterface
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VPCEndpointType = VPCEndpointType' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern VETGateway :: VPCEndpointType
pattern VETGateway = VPCEndpointType' "Gateway"

pattern VETInterface :: VPCEndpointType
pattern VETInterface = VPCEndpointType' "Interface"

{-# COMPLETE
  VETGateway,
  VETInterface,
  VPCEndpointType' #-}

instance FromText VPCEndpointType where
    parser = (VPCEndpointType' . mk) <$> takeText

instance ToText VPCEndpointType where
    toText (VPCEndpointType' ci) = original ci

-- | Represents an enum of /known/ $VPCEndpointType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum VPCEndpointType where
    toEnum i = case i of
        0 -> VETGateway
        1 -> VETInterface
        _ -> (error . showText) $ "Unknown index for VPCEndpointType: " <> toText i
    fromEnum x = case x of
        VETGateway -> 0
        VETInterface -> 1
        VPCEndpointType' name -> (error . showText) $ "Unknown VPCEndpointType: " <> original name

-- | Represents the bounds of /known/ $VPCEndpointType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded VPCEndpointType where
    minBound = VETGateway
    maxBound = VETInterface

instance Hashable     VPCEndpointType
instance NFData       VPCEndpointType
instance ToByteString VPCEndpointType
instance ToQuery      VPCEndpointType
instance ToHeader     VPCEndpointType

instance FromXML VPCEndpointType where
    parseXML = parseXMLText "VPCEndpointType"
