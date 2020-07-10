{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceNetworkInterfaceAssociation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InstanceNetworkInterfaceAssociation where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes association information for an Elastic IP address (IPv4).
--
--
--
-- /See:/ 'instanceNetworkInterfaceAssociation' smart constructor.
data InstanceNetworkInterfaceAssociation = InstanceNetworkInterfaceAssociation'{_iniaPublicDNSName
                                                                                ::
                                                                                !(Maybe
                                                                                    Text),
                                                                                _iniaIPOwnerId
                                                                                ::
                                                                                !(Maybe
                                                                                    Text),
                                                                                _iniaPublicIP
                                                                                ::
                                                                                !(Maybe
                                                                                    Text)}
                                             deriving (Eq, Read, Show, Data,
                                                       Typeable, Generic)

-- | Creates a value of 'InstanceNetworkInterfaceAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iniaPublicDNSName' - The public DNS name.
--
-- * 'iniaIPOwnerId' - The ID of the owner of the Elastic IP address.
--
-- * 'iniaPublicIP' - The public IP address or Elastic IP address bound to the network interface.
instanceNetworkInterfaceAssociation
    :: InstanceNetworkInterfaceAssociation
instanceNetworkInterfaceAssociation
  = InstanceNetworkInterfaceAssociation'{_iniaPublicDNSName
                                           = Nothing,
                                         _iniaIPOwnerId = Nothing,
                                         _iniaPublicIP = Nothing}

-- | The public DNS name.
iniaPublicDNSName :: Lens' InstanceNetworkInterfaceAssociation (Maybe Text)
iniaPublicDNSName = lens _iniaPublicDNSName (\ s a -> s{_iniaPublicDNSName = a})

-- | The ID of the owner of the Elastic IP address.
iniaIPOwnerId :: Lens' InstanceNetworkInterfaceAssociation (Maybe Text)
iniaIPOwnerId = lens _iniaIPOwnerId (\ s a -> s{_iniaIPOwnerId = a})

-- | The public IP address or Elastic IP address bound to the network interface.
iniaPublicIP :: Lens' InstanceNetworkInterfaceAssociation (Maybe Text)
iniaPublicIP = lens _iniaPublicIP (\ s a -> s{_iniaPublicIP = a})

instance FromXML InstanceNetworkInterfaceAssociation
         where
        parseXML x
          = InstanceNetworkInterfaceAssociation' <$>
              (x .@? "publicDnsName") <*> (x .@? "ipOwnerId") <*>
                (x .@? "publicIp")

instance Hashable InstanceNetworkInterfaceAssociation
         where

instance NFData InstanceNetworkInterfaceAssociation
         where
