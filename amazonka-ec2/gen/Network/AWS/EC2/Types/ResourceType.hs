{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ResourceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ResourceType (
  ResourceType (
    ..
    , RTClientVPNEndpoint
    , RTCustomerGateway
    , RTDHCPOptions
    , RTDedicatedHost
    , RTElasticIP
    , RTFleet
    , RTFpgaImage
    , RTHostReservation
    , RTImage
    , RTInstance
    , RTInternetGateway
    , RTKeyPair
    , RTLaunchTemplate
    , RTNatgateway
    , RTNetworkACL
    , RTNetworkInterface
    , RTPlacementGroup
    , RTReservedInstances
    , RTRouteTable
    , RTSecurityGroup
    , RTSnapshot
    , RTSpotFleetRequest
    , RTSpotInstancesRequest
    , RTSubnet
    , RTTrafficMirrorFilter
    , RTTrafficMirrorSession
    , RTTrafficMirrorTarget
    , RTTransitGateway
    , RTTransitGatewayAttachment
    , RTTransitGatewayMulticastDomain
    , RTTransitGatewayRouteTable
    , RTVPC
    , RTVPCFlowLog
    , RTVPCPeeringConnection
    , RTVPNConnection
    , RTVPNGateway
    , RTVolume
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ResourceType = ResourceType' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern RTClientVPNEndpoint :: ResourceType
pattern RTClientVPNEndpoint = ResourceType' "client-vpn-endpoint"

pattern RTCustomerGateway :: ResourceType
pattern RTCustomerGateway = ResourceType' "customer-gateway"

pattern RTDHCPOptions :: ResourceType
pattern RTDHCPOptions = ResourceType' "dhcp-options"

pattern RTDedicatedHost :: ResourceType
pattern RTDedicatedHost = ResourceType' "dedicated-host"

pattern RTElasticIP :: ResourceType
pattern RTElasticIP = ResourceType' "elastic-ip"

pattern RTFleet :: ResourceType
pattern RTFleet = ResourceType' "fleet"

pattern RTFpgaImage :: ResourceType
pattern RTFpgaImage = ResourceType' "fpga-image"

pattern RTHostReservation :: ResourceType
pattern RTHostReservation = ResourceType' "host-reservation"

pattern RTImage :: ResourceType
pattern RTImage = ResourceType' "image"

pattern RTInstance :: ResourceType
pattern RTInstance = ResourceType' "instance"

pattern RTInternetGateway :: ResourceType
pattern RTInternetGateway = ResourceType' "internet-gateway"

pattern RTKeyPair :: ResourceType
pattern RTKeyPair = ResourceType' "key-pair"

pattern RTLaunchTemplate :: ResourceType
pattern RTLaunchTemplate = ResourceType' "launch-template"

pattern RTNatgateway :: ResourceType
pattern RTNatgateway = ResourceType' "natgateway"

pattern RTNetworkACL :: ResourceType
pattern RTNetworkACL = ResourceType' "network-acl"

pattern RTNetworkInterface :: ResourceType
pattern RTNetworkInterface = ResourceType' "network-interface"

pattern RTPlacementGroup :: ResourceType
pattern RTPlacementGroup = ResourceType' "placement-group"

pattern RTReservedInstances :: ResourceType
pattern RTReservedInstances = ResourceType' "reserved-instances"

pattern RTRouteTable :: ResourceType
pattern RTRouteTable = ResourceType' "route-table"

pattern RTSecurityGroup :: ResourceType
pattern RTSecurityGroup = ResourceType' "security-group"

pattern RTSnapshot :: ResourceType
pattern RTSnapshot = ResourceType' "snapshot"

pattern RTSpotFleetRequest :: ResourceType
pattern RTSpotFleetRequest = ResourceType' "spot-fleet-request"

pattern RTSpotInstancesRequest :: ResourceType
pattern RTSpotInstancesRequest = ResourceType' "spot-instances-request"

pattern RTSubnet :: ResourceType
pattern RTSubnet = ResourceType' "subnet"

pattern RTTrafficMirrorFilter :: ResourceType
pattern RTTrafficMirrorFilter = ResourceType' "traffic-mirror-filter"

pattern RTTrafficMirrorSession :: ResourceType
pattern RTTrafficMirrorSession = ResourceType' "traffic-mirror-session"

pattern RTTrafficMirrorTarget :: ResourceType
pattern RTTrafficMirrorTarget = ResourceType' "traffic-mirror-target"

pattern RTTransitGateway :: ResourceType
pattern RTTransitGateway = ResourceType' "transit-gateway"

pattern RTTransitGatewayAttachment :: ResourceType
pattern RTTransitGatewayAttachment = ResourceType' "transit-gateway-attachment"

pattern RTTransitGatewayMulticastDomain :: ResourceType
pattern RTTransitGatewayMulticastDomain = ResourceType' "transit-gateway-multicast-domain"

pattern RTTransitGatewayRouteTable :: ResourceType
pattern RTTransitGatewayRouteTable = ResourceType' "transit-gateway-route-table"

pattern RTVPC :: ResourceType
pattern RTVPC = ResourceType' "vpc"

pattern RTVPCFlowLog :: ResourceType
pattern RTVPCFlowLog = ResourceType' "vpc-flow-log"

pattern RTVPCPeeringConnection :: ResourceType
pattern RTVPCPeeringConnection = ResourceType' "vpc-peering-connection"

pattern RTVPNConnection :: ResourceType
pattern RTVPNConnection = ResourceType' "vpn-connection"

pattern RTVPNGateway :: ResourceType
pattern RTVPNGateway = ResourceType' "vpn-gateway"

pattern RTVolume :: ResourceType
pattern RTVolume = ResourceType' "volume"

{-# COMPLETE
  RTClientVPNEndpoint,
  RTCustomerGateway,
  RTDHCPOptions,
  RTDedicatedHost,
  RTElasticIP,
  RTFleet,
  RTFpgaImage,
  RTHostReservation,
  RTImage,
  RTInstance,
  RTInternetGateway,
  RTKeyPair,
  RTLaunchTemplate,
  RTNatgateway,
  RTNetworkACL,
  RTNetworkInterface,
  RTPlacementGroup,
  RTReservedInstances,
  RTRouteTable,
  RTSecurityGroup,
  RTSnapshot,
  RTSpotFleetRequest,
  RTSpotInstancesRequest,
  RTSubnet,
  RTTrafficMirrorFilter,
  RTTrafficMirrorSession,
  RTTrafficMirrorTarget,
  RTTransitGateway,
  RTTransitGatewayAttachment,
  RTTransitGatewayMulticastDomain,
  RTTransitGatewayRouteTable,
  RTVPC,
  RTVPCFlowLog,
  RTVPCPeeringConnection,
  RTVPNConnection,
  RTVPNGateway,
  RTVolume,
  ResourceType' #-}

instance FromText ResourceType where
    parser = (ResourceType' . mk) <$> takeText

instance ToText ResourceType where
    toText (ResourceType' ci) = original ci

-- | Represents an enum of /known/ $ResourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ResourceType where
    toEnum i = case i of
        0 -> RTClientVPNEndpoint
        1 -> RTCustomerGateway
        2 -> RTDHCPOptions
        3 -> RTDedicatedHost
        4 -> RTElasticIP
        5 -> RTFleet
        6 -> RTFpgaImage
        7 -> RTHostReservation
        8 -> RTImage
        9 -> RTInstance
        10 -> RTInternetGateway
        11 -> RTKeyPair
        12 -> RTLaunchTemplate
        13 -> RTNatgateway
        14 -> RTNetworkACL
        15 -> RTNetworkInterface
        16 -> RTPlacementGroup
        17 -> RTReservedInstances
        18 -> RTRouteTable
        19 -> RTSecurityGroup
        20 -> RTSnapshot
        21 -> RTSpotFleetRequest
        22 -> RTSpotInstancesRequest
        23 -> RTSubnet
        24 -> RTTrafficMirrorFilter
        25 -> RTTrafficMirrorSession
        26 -> RTTrafficMirrorTarget
        27 -> RTTransitGateway
        28 -> RTTransitGatewayAttachment
        29 -> RTTransitGatewayMulticastDomain
        30 -> RTTransitGatewayRouteTable
        31 -> RTVPC
        32 -> RTVPCFlowLog
        33 -> RTVPCPeeringConnection
        34 -> RTVPNConnection
        35 -> RTVPNGateway
        36 -> RTVolume
        _ -> (error . showText) $ "Unknown index for ResourceType: " <> toText i
    fromEnum x = case x of
        RTClientVPNEndpoint -> 0
        RTCustomerGateway -> 1
        RTDHCPOptions -> 2
        RTDedicatedHost -> 3
        RTElasticIP -> 4
        RTFleet -> 5
        RTFpgaImage -> 6
        RTHostReservation -> 7
        RTImage -> 8
        RTInstance -> 9
        RTInternetGateway -> 10
        RTKeyPair -> 11
        RTLaunchTemplate -> 12
        RTNatgateway -> 13
        RTNetworkACL -> 14
        RTNetworkInterface -> 15
        RTPlacementGroup -> 16
        RTReservedInstances -> 17
        RTRouteTable -> 18
        RTSecurityGroup -> 19
        RTSnapshot -> 20
        RTSpotFleetRequest -> 21
        RTSpotInstancesRequest -> 22
        RTSubnet -> 23
        RTTrafficMirrorFilter -> 24
        RTTrafficMirrorSession -> 25
        RTTrafficMirrorTarget -> 26
        RTTransitGateway -> 27
        RTTransitGatewayAttachment -> 28
        RTTransitGatewayMulticastDomain -> 29
        RTTransitGatewayRouteTable -> 30
        RTVPC -> 31
        RTVPCFlowLog -> 32
        RTVPCPeeringConnection -> 33
        RTVPNConnection -> 34
        RTVPNGateway -> 35
        RTVolume -> 36
        ResourceType' name -> (error . showText) $ "Unknown ResourceType: " <> original name

-- | Represents the bounds of /known/ $ResourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ResourceType where
    minBound = RTClientVPNEndpoint
    maxBound = RTVolume

instance Hashable     ResourceType
instance NFData       ResourceType
instance ToByteString ResourceType
instance ToQuery      ResourceType
instance ToHeader     ResourceType

instance FromXML ResourceType where
    parseXML = parseXMLText "ResourceType"
