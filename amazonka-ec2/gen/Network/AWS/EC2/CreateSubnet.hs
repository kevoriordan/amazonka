{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateSubnet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a subnet in an existing VPC.
--
--
-- When you create each subnet, you provide the VPC ID and IPv4 CIDR block for the subnet. After you create a subnet, you can't change its CIDR block. The size of the subnet's IPv4 CIDR block can be the same as a VPC's IPv4 CIDR block, or a subset of a VPC's IPv4 CIDR block. If you create more than one subnet in a VPC, the subnets' CIDR blocks must not overlap. The smallest IPv4 subnet (and VPC) you can create uses a /28 netmask (16 IPv4 addresses), and the largest uses a /16 netmask (65,536 IPv4 addresses).
--
-- If you've associated an IPv6 CIDR block with your VPC, you can create a subnet with an IPv6 CIDR block that uses a /64 prefix length. 
--
-- /Important:/ AWS reserves both the first four and the last IPv4 address in each subnet's CIDR block. They're not available for use.
--
-- If you add more than one subnet to a VPC, they're set up in a star topology with a logical router in the middle.
--
-- If you launch an instance in a VPC using an Amazon EBS-backed AMI, the IP address doesn't change if you stop and restart the instance (unlike a similar instance launched outside a VPC, which gets a new IP address when restarted). It's therefore possible to have a subnet with no running instances (they're all stopped), but no remaining IP addresses available.
--
-- For more information about subnets, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html Your VPC and Subnets> in the /Amazon Virtual Private Cloud User Guide/ .
--
module Network.AWS.EC2.CreateSubnet
    (
    -- * Creating a Request
      createSubnet
    , CreateSubnet
    -- * Request Lenses
    , crtsbntIPv6CidrBlock
    , crtsbntAvailabilityZoneId
    , crtsbntOutpostARN
    , crtsbntAvailabilityZone
    , crtsbntDryRun
    , crtsbntCidrBlock
    , crtsbntVPCId

    -- * Destructuring the Response
    , createSubnetResponse
    , CreateSubnetResponse
    -- * Response Lenses
    , crersSubnet
    , crersResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createSubnet' smart constructor.
data CreateSubnet = CreateSubnet'{_crtsbntIPv6CidrBlock
                                  :: !(Maybe Text),
                                  _crtsbntAvailabilityZoneId :: !(Maybe Text),
                                  _crtsbntOutpostARN :: !(Maybe Text),
                                  _crtsbntAvailabilityZone :: !(Maybe Text),
                                  _crtsbntDryRun :: !(Maybe Bool),
                                  _crtsbntCidrBlock :: !Text,
                                  _crtsbntVPCId :: !Text}
                      deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateSubnet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crtsbntIPv6CidrBlock' - The IPv6 network range for the subnet, in CIDR notation. The subnet size must use a /64 prefix length.
--
-- * 'crtsbntAvailabilityZoneId' - The AZ ID or the Local Zone ID of the subnet.
--
-- * 'crtsbntOutpostARN' - The Amazon Resource Name (ARN) of the Outpost.
--
-- * 'crtsbntAvailabilityZone' - The Availability Zone or Local Zone for the subnet. Default: AWS selects one for you. If you create more than one subnet in your VPC, we do not necessarily select a different zone for each subnet. To create a subnet in a Local Zone, set this value to the Local Zone ID, for example @us-west-2-lax-1a@ . For information about the Regions that support Local Zones, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-regions-availability-zones.html#concepts-available-regions Available Regions> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'crtsbntDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'crtsbntCidrBlock' - The IPv4 network range for the subnet, in CIDR notation. For example, @10.0.0.0/24@ .
--
-- * 'crtsbntVPCId' - The ID of the VPC.
createSubnet
    :: Text -- ^ 'crtsbntCidrBlock'
    -> Text -- ^ 'crtsbntVPCId'
    -> CreateSubnet
createSubnet pCidrBlock_ pVPCId_
  = CreateSubnet'{_crtsbntIPv6CidrBlock = Nothing,
                  _crtsbntAvailabilityZoneId = Nothing,
                  _crtsbntOutpostARN = Nothing,
                  _crtsbntAvailabilityZone = Nothing,
                  _crtsbntDryRun = Nothing,
                  _crtsbntCidrBlock = pCidrBlock_,
                  _crtsbntVPCId = pVPCId_}

-- | The IPv6 network range for the subnet, in CIDR notation. The subnet size must use a /64 prefix length.
crtsbntIPv6CidrBlock :: Lens' CreateSubnet (Maybe Text)
crtsbntIPv6CidrBlock = lens _crtsbntIPv6CidrBlock (\ s a -> s{_crtsbntIPv6CidrBlock = a})

-- | The AZ ID or the Local Zone ID of the subnet.
crtsbntAvailabilityZoneId :: Lens' CreateSubnet (Maybe Text)
crtsbntAvailabilityZoneId = lens _crtsbntAvailabilityZoneId (\ s a -> s{_crtsbntAvailabilityZoneId = a})

-- | The Amazon Resource Name (ARN) of the Outpost.
crtsbntOutpostARN :: Lens' CreateSubnet (Maybe Text)
crtsbntOutpostARN = lens _crtsbntOutpostARN (\ s a -> s{_crtsbntOutpostARN = a})

-- | The Availability Zone or Local Zone for the subnet. Default: AWS selects one for you. If you create more than one subnet in your VPC, we do not necessarily select a different zone for each subnet. To create a subnet in a Local Zone, set this value to the Local Zone ID, for example @us-west-2-lax-1a@ . For information about the Regions that support Local Zones, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-regions-availability-zones.html#concepts-available-regions Available Regions> in the /Amazon Elastic Compute Cloud User Guide/ .
crtsbntAvailabilityZone :: Lens' CreateSubnet (Maybe Text)
crtsbntAvailabilityZone = lens _crtsbntAvailabilityZone (\ s a -> s{_crtsbntAvailabilityZone = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
crtsbntDryRun :: Lens' CreateSubnet (Maybe Bool)
crtsbntDryRun = lens _crtsbntDryRun (\ s a -> s{_crtsbntDryRun = a})

-- | The IPv4 network range for the subnet, in CIDR notation. For example, @10.0.0.0/24@ .
crtsbntCidrBlock :: Lens' CreateSubnet Text
crtsbntCidrBlock = lens _crtsbntCidrBlock (\ s a -> s{_crtsbntCidrBlock = a})

-- | The ID of the VPC.
crtsbntVPCId :: Lens' CreateSubnet Text
crtsbntVPCId = lens _crtsbntVPCId (\ s a -> s{_crtsbntVPCId = a})

instance AWSRequest CreateSubnet where
        type Rs CreateSubnet = CreateSubnetResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateSubnetResponse' <$>
                   (x .@? "subnet") <*> (pure (fromEnum s)))

instance Hashable CreateSubnet where

instance NFData CreateSubnet where

instance ToHeaders CreateSubnet where
        toHeaders = const mempty

instance ToPath CreateSubnet where
        toPath = const "/"

instance ToQuery CreateSubnet where
        toQuery CreateSubnet'{..}
          = mconcat
              ["Action" =: ("CreateSubnet" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "Ipv6CidrBlock" =: _crtsbntIPv6CidrBlock,
               "AvailabilityZoneId" =: _crtsbntAvailabilityZoneId,
               "OutpostArn" =: _crtsbntOutpostARN,
               "AvailabilityZone" =: _crtsbntAvailabilityZone,
               "DryRun" =: _crtsbntDryRun,
               "CidrBlock" =: _crtsbntCidrBlock,
               "VpcId" =: _crtsbntVPCId]

-- | /See:/ 'createSubnetResponse' smart constructor.
data CreateSubnetResponse = CreateSubnetResponse'{_crersSubnet
                                                  :: !(Maybe Subnet),
                                                  _crersResponseStatus :: !Int}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateSubnetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crersSubnet' - Information about the subnet.
--
-- * 'crersResponseStatus' - -- | The response status code.
createSubnetResponse
    :: Int -- ^ 'crersResponseStatus'
    -> CreateSubnetResponse
createSubnetResponse pResponseStatus_
  = CreateSubnetResponse'{_crersSubnet = Nothing,
                          _crersResponseStatus = pResponseStatus_}

-- | Information about the subnet.
crersSubnet :: Lens' CreateSubnetResponse (Maybe Subnet)
crersSubnet = lens _crersSubnet (\ s a -> s{_crersSubnet = a})

-- | -- | The response status code.
crersResponseStatus :: Lens' CreateSubnetResponse Int
crersResponseStatus = lens _crersResponseStatus (\ s a -> s{_crersResponseStatus = a})

instance NFData CreateSubnetResponse where
