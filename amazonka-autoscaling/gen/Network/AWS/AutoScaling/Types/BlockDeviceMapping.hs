{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.BlockDeviceMapping
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.BlockDeviceMapping where

import Network.AWS.AutoScaling.Types.EBS
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a block device mapping.
--
--
--
-- /See:/ 'blockDeviceMapping' smart constructor.
data BlockDeviceMapping = BlockDeviceMapping'{_bdmVirtualName
                                              :: !(Maybe Text),
                                              _bdmNoDevice :: !(Maybe Bool),
                                              _bdmEBS :: !(Maybe EBS),
                                              _bdmDeviceName :: !Text}
                            deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BlockDeviceMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdmVirtualName' - The name of the virtual device (for example, @ephemeral0@ ).
--
-- * 'bdmNoDevice' - Suppresses a device mapping. If this parameter is true for the root device, the instance might fail the EC2 health check. In that case, Amazon EC2 Auto Scaling launches a replacement instance.
--
-- * 'bdmEBS' - The information about the Amazon EBS volume.
--
-- * 'bdmDeviceName' - The device name exposed to the EC2 instance (for example, @/dev/sdh@ or @xvdh@ ). For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/device_naming.html Device Naming on Linux Instances> in the /Amazon EC2 User Guide for Linux Instances/ .
blockDeviceMapping
    :: Text -- ^ 'bdmDeviceName'
    -> BlockDeviceMapping
blockDeviceMapping pDeviceName_
  = BlockDeviceMapping'{_bdmVirtualName = Nothing,
                        _bdmNoDevice = Nothing, _bdmEBS = Nothing,
                        _bdmDeviceName = pDeviceName_}

-- | The name of the virtual device (for example, @ephemeral0@ ).
bdmVirtualName :: Lens' BlockDeviceMapping (Maybe Text)
bdmVirtualName = lens _bdmVirtualName (\ s a -> s{_bdmVirtualName = a})

-- | Suppresses a device mapping. If this parameter is true for the root device, the instance might fail the EC2 health check. In that case, Amazon EC2 Auto Scaling launches a replacement instance.
bdmNoDevice :: Lens' BlockDeviceMapping (Maybe Bool)
bdmNoDevice = lens _bdmNoDevice (\ s a -> s{_bdmNoDevice = a})

-- | The information about the Amazon EBS volume.
bdmEBS :: Lens' BlockDeviceMapping (Maybe EBS)
bdmEBS = lens _bdmEBS (\ s a -> s{_bdmEBS = a})

-- | The device name exposed to the EC2 instance (for example, @/dev/sdh@ or @xvdh@ ). For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/device_naming.html Device Naming on Linux Instances> in the /Amazon EC2 User Guide for Linux Instances/ .
bdmDeviceName :: Lens' BlockDeviceMapping Text
bdmDeviceName = lens _bdmDeviceName (\ s a -> s{_bdmDeviceName = a})

instance FromXML BlockDeviceMapping where
        parseXML x
          = BlockDeviceMapping' <$>
              (x .@? "VirtualName") <*> (x .@? "NoDevice") <*>
                (x .@? "Ebs")
                <*> (x .@ "DeviceName")

instance Hashable BlockDeviceMapping where

instance NFData BlockDeviceMapping where

instance ToQuery BlockDeviceMapping where
        toQuery BlockDeviceMapping'{..}
          = mconcat
              ["VirtualName" =: _bdmVirtualName,
               "NoDevice" =: _bdmNoDevice, "Ebs" =: _bdmEBS,
               "DeviceName" =: _bdmDeviceName]
