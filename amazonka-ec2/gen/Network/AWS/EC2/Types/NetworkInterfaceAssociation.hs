{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfaceAssociation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.NetworkInterfaceAssociation where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes association information for an Elastic IP address (IPv4 only).
--
--
--
-- /See:/ 'networkInterfaceAssociation' smart constructor.
data NetworkInterfaceAssociation = NetworkInterfaceAssociation'{_niaAssociationId
                                                                ::
                                                                !(Maybe Text),
                                                                _niaPublicDNSName
                                                                ::
                                                                !(Maybe Text),
                                                                _niaAllocationId
                                                                ::
                                                                !(Maybe Text),
                                                                _niaIPOwnerId ::
                                                                !(Maybe Text),
                                                                _niaPublicIP ::
                                                                !(Maybe Text)}
                                     deriving (Eq, Read, Show, Data, Typeable,
                                               Generic)

-- | Creates a value of 'NetworkInterfaceAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'niaAssociationId' - The association ID.
--
-- * 'niaPublicDNSName' - The public DNS name.
--
-- * 'niaAllocationId' - The allocation ID.
--
-- * 'niaIPOwnerId' - The ID of the Elastic IP address owner.
--
-- * 'niaPublicIP' - The address of the Elastic IP address bound to the network interface.
networkInterfaceAssociation
    :: NetworkInterfaceAssociation
networkInterfaceAssociation
  = NetworkInterfaceAssociation'{_niaAssociationId =
                                   Nothing,
                                 _niaPublicDNSName = Nothing,
                                 _niaAllocationId = Nothing,
                                 _niaIPOwnerId = Nothing,
                                 _niaPublicIP = Nothing}

-- | The association ID.
niaAssociationId :: Lens' NetworkInterfaceAssociation (Maybe Text)
niaAssociationId = lens _niaAssociationId (\ s a -> s{_niaAssociationId = a})

-- | The public DNS name.
niaPublicDNSName :: Lens' NetworkInterfaceAssociation (Maybe Text)
niaPublicDNSName = lens _niaPublicDNSName (\ s a -> s{_niaPublicDNSName = a})

-- | The allocation ID.
niaAllocationId :: Lens' NetworkInterfaceAssociation (Maybe Text)
niaAllocationId = lens _niaAllocationId (\ s a -> s{_niaAllocationId = a})

-- | The ID of the Elastic IP address owner.
niaIPOwnerId :: Lens' NetworkInterfaceAssociation (Maybe Text)
niaIPOwnerId = lens _niaIPOwnerId (\ s a -> s{_niaIPOwnerId = a})

-- | The address of the Elastic IP address bound to the network interface.
niaPublicIP :: Lens' NetworkInterfaceAssociation (Maybe Text)
niaPublicIP = lens _niaPublicIP (\ s a -> s{_niaPublicIP = a})

instance FromXML NetworkInterfaceAssociation where
        parseXML x
          = NetworkInterfaceAssociation' <$>
              (x .@? "associationId") <*> (x .@? "publicDnsName")
                <*> (x .@? "allocationId")
                <*> (x .@? "ipOwnerId")
                <*> (x .@? "publicIp")

instance Hashable NetworkInterfaceAssociation where

instance NFData NetworkInterfaceAssociation where
