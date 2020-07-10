{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.Subnet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.Subnet where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.AvailabilityZone

-- | This data type is used as a response element in the @DescribeDBSubnetGroups@ action. 
--
--
--
-- /See:/ 'subnet' smart constructor.
data Subnet = Subnet'{_sSubnetStatus ::
                      !(Maybe Text),
                      _sSubnetIdentifier :: !(Maybe Text),
                      _sSubnetAvailabilityZone ::
                      !(Maybe AvailabilityZone)}
                deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Subnet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sSubnetStatus' - Specifies the status of the subnet.
--
-- * 'sSubnetIdentifier' - Specifies the identifier of the subnet.
--
-- * 'sSubnetAvailabilityZone' - Undocumented member.
subnet
    :: Subnet
subnet
  = Subnet'{_sSubnetStatus = Nothing,
            _sSubnetIdentifier = Nothing,
            _sSubnetAvailabilityZone = Nothing}

-- | Specifies the status of the subnet.
sSubnetStatus :: Lens' Subnet (Maybe Text)
sSubnetStatus = lens _sSubnetStatus (\ s a -> s{_sSubnetStatus = a})

-- | Specifies the identifier of the subnet.
sSubnetIdentifier :: Lens' Subnet (Maybe Text)
sSubnetIdentifier = lens _sSubnetIdentifier (\ s a -> s{_sSubnetIdentifier = a})

-- | Undocumented member.
sSubnetAvailabilityZone :: Lens' Subnet (Maybe AvailabilityZone)
sSubnetAvailabilityZone = lens _sSubnetAvailabilityZone (\ s a -> s{_sSubnetAvailabilityZone = a})

instance FromXML Subnet where
        parseXML x
          = Subnet' <$>
              (x .@? "SubnetStatus") <*> (x .@? "SubnetIdentifier")
                <*> (x .@? "SubnetAvailabilityZone")

instance Hashable Subnet where

instance NFData Subnet where
