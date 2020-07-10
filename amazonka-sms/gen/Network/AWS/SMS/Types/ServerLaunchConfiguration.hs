{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ServerLaunchConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types.ServerLaunchConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SMS.Types.Server
import Network.AWS.SMS.Types.UserData

-- | Launch configuration for a server.
--
--
--
-- /See:/ 'serverLaunchConfiguration' smart constructor.
data ServerLaunchConfiguration = ServerLaunchConfiguration'{_slcEc2KeyName
                                                            :: !(Maybe Text),
                                                            _slcAssociatePublicIPAddress
                                                            :: !(Maybe Bool),
                                                            _slcSubnet ::
                                                            !(Maybe Text),
                                                            _slcLogicalId ::
                                                            !(Maybe Text),
                                                            _slcSecurityGroup ::
                                                            !(Maybe Text),
                                                            _slcUserData ::
                                                            !(Maybe UserData),
                                                            _slcInstanceType ::
                                                            !(Maybe Text),
                                                            _slcServer ::
                                                            !(Maybe Server),
                                                            _slcVpc ::
                                                            !(Maybe Text)}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'ServerLaunchConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slcEc2KeyName' - Name of the EC2 SSH Key to be used for connecting to the launched server.
--
-- * 'slcAssociatePublicIPAddress' - If true, a publicly accessible IP address is created when launching the server.
--
-- * 'slcSubnet' - Identifier of the subnet the server should be launched into.
--
-- * 'slcLogicalId' - Logical ID of the server in the Amazon CloudFormation template.
--
-- * 'slcSecurityGroup' - Identifier of the security group that applies to the launched server.
--
-- * 'slcUserData' - Location of the user-data script to be executed when launching the server.
--
-- * 'slcInstanceType' - Instance type to be used for launching the server.
--
-- * 'slcServer' - Identifier of the server the launch configuration is associated with.
--
-- * 'slcVpc' - Identifier of the VPC the server should be launched into.
serverLaunchConfiguration
    :: ServerLaunchConfiguration
serverLaunchConfiguration
  = ServerLaunchConfiguration'{_slcEc2KeyName =
                                 Nothing,
                               _slcAssociatePublicIPAddress = Nothing,
                               _slcSubnet = Nothing, _slcLogicalId = Nothing,
                               _slcSecurityGroup = Nothing,
                               _slcUserData = Nothing,
                               _slcInstanceType = Nothing, _slcServer = Nothing,
                               _slcVpc = Nothing}

-- | Name of the EC2 SSH Key to be used for connecting to the launched server.
slcEc2KeyName :: Lens' ServerLaunchConfiguration (Maybe Text)
slcEc2KeyName = lens _slcEc2KeyName (\ s a -> s{_slcEc2KeyName = a})

-- | If true, a publicly accessible IP address is created when launching the server.
slcAssociatePublicIPAddress :: Lens' ServerLaunchConfiguration (Maybe Bool)
slcAssociatePublicIPAddress = lens _slcAssociatePublicIPAddress (\ s a -> s{_slcAssociatePublicIPAddress = a})

-- | Identifier of the subnet the server should be launched into.
slcSubnet :: Lens' ServerLaunchConfiguration (Maybe Text)
slcSubnet = lens _slcSubnet (\ s a -> s{_slcSubnet = a})

-- | Logical ID of the server in the Amazon CloudFormation template.
slcLogicalId :: Lens' ServerLaunchConfiguration (Maybe Text)
slcLogicalId = lens _slcLogicalId (\ s a -> s{_slcLogicalId = a})

-- | Identifier of the security group that applies to the launched server.
slcSecurityGroup :: Lens' ServerLaunchConfiguration (Maybe Text)
slcSecurityGroup = lens _slcSecurityGroup (\ s a -> s{_slcSecurityGroup = a})

-- | Location of the user-data script to be executed when launching the server.
slcUserData :: Lens' ServerLaunchConfiguration (Maybe UserData)
slcUserData = lens _slcUserData (\ s a -> s{_slcUserData = a})

-- | Instance type to be used for launching the server.
slcInstanceType :: Lens' ServerLaunchConfiguration (Maybe Text)
slcInstanceType = lens _slcInstanceType (\ s a -> s{_slcInstanceType = a})

-- | Identifier of the server the launch configuration is associated with.
slcServer :: Lens' ServerLaunchConfiguration (Maybe Server)
slcServer = lens _slcServer (\ s a -> s{_slcServer = a})

-- | Identifier of the VPC the server should be launched into.
slcVpc :: Lens' ServerLaunchConfiguration (Maybe Text)
slcVpc = lens _slcVpc (\ s a -> s{_slcVpc = a})

instance FromJSON ServerLaunchConfiguration where
        parseJSON
          = withObject "ServerLaunchConfiguration"
              (\ x ->
                 ServerLaunchConfiguration' <$>
                   (x .:? "ec2KeyName") <*>
                     (x .:? "associatePublicIpAddress")
                     <*> (x .:? "subnet")
                     <*> (x .:? "logicalId")
                     <*> (x .:? "securityGroup")
                     <*> (x .:? "userData")
                     <*> (x .:? "instanceType")
                     <*> (x .:? "server")
                     <*> (x .:? "vpc"))

instance Hashable ServerLaunchConfiguration where

instance NFData ServerLaunchConfiguration where

instance ToJSON ServerLaunchConfiguration where
        toJSON ServerLaunchConfiguration'{..}
          = object
              (catMaybes
                 [("ec2KeyName" .=) <$> _slcEc2KeyName,
                  ("associatePublicIpAddress" .=) <$>
                    _slcAssociatePublicIPAddress,
                  ("subnet" .=) <$> _slcSubnet,
                  ("logicalId" .=) <$> _slcLogicalId,
                  ("securityGroup" .=) <$> _slcSecurityGroup,
                  ("userData" .=) <$> _slcUserData,
                  ("instanceType" .=) <$> _slcInstanceType,
                  ("server" .=) <$> _slcServer,
                  ("vpc" .=) <$> _slcVpc])
