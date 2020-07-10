{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType (
  TransitGatewayAttachmentResourceType (
    ..
    , DirectConnectGateway
    , TgwPeering
    , VPC
    , VPN
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TransitGatewayAttachmentResourceType = TransitGatewayAttachmentResourceType' (CI
                                                                                     Text)
                                              deriving (Eq, Ord, Read, Show,
                                                        Data, Typeable, Generic)

pattern DirectConnectGateway :: TransitGatewayAttachmentResourceType
pattern DirectConnectGateway = TransitGatewayAttachmentResourceType' "direct-connect-gateway"

pattern TgwPeering :: TransitGatewayAttachmentResourceType
pattern TgwPeering = TransitGatewayAttachmentResourceType' "tgw-peering"

pattern VPC :: TransitGatewayAttachmentResourceType
pattern VPC = TransitGatewayAttachmentResourceType' "vpc"

pattern VPN :: TransitGatewayAttachmentResourceType
pattern VPN = TransitGatewayAttachmentResourceType' "vpn"

{-# COMPLETE
  DirectConnectGateway,
  TgwPeering,
  VPC,
  VPN,
  TransitGatewayAttachmentResourceType' #-}

instance FromText TransitGatewayAttachmentResourceType where
    parser = (TransitGatewayAttachmentResourceType' . mk) <$> takeText

instance ToText TransitGatewayAttachmentResourceType where
    toText (TransitGatewayAttachmentResourceType' ci) = original ci

-- | Represents an enum of /known/ $TransitGatewayAttachmentResourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TransitGatewayAttachmentResourceType where
    toEnum i = case i of
        0 -> DirectConnectGateway
        1 -> TgwPeering
        2 -> VPC
        3 -> VPN
        _ -> (error . showText) $ "Unknown index for TransitGatewayAttachmentResourceType: " <> toText i
    fromEnum x = case x of
        DirectConnectGateway -> 0
        TgwPeering -> 1
        VPC -> 2
        VPN -> 3
        TransitGatewayAttachmentResourceType' name -> (error . showText) $ "Unknown TransitGatewayAttachmentResourceType: " <> original name

-- | Represents the bounds of /known/ $TransitGatewayAttachmentResourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TransitGatewayAttachmentResourceType where
    minBound = DirectConnectGateway
    maxBound = VPN

instance Hashable     TransitGatewayAttachmentResourceType
instance NFData       TransitGatewayAttachmentResourceType
instance ToByteString TransitGatewayAttachmentResourceType
instance ToQuery      TransitGatewayAttachmentResourceType
instance ToHeader     TransitGatewayAttachmentResourceType

instance FromXML TransitGatewayAttachmentResourceType where
    parseXML = parseXMLText "TransitGatewayAttachmentResourceType"
