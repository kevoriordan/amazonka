{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInfo
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.NetworkInfo where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.EnaSupport
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the networking features of the instance type.
--
--
--
-- /See:/ 'networkInfo' smart constructor.
data NetworkInfo = NetworkInfo'{_niIPv6Supported ::
                                !(Maybe Bool),
                                _niEnaSupport :: !(Maybe EnaSupport),
                                _niMaximumNetworkInterfaces :: !(Maybe Int),
                                _niIPv6AddressesPerInterface :: !(Maybe Int),
                                _niNetworkPerformance :: !(Maybe Text),
                                _niIPv4AddressesPerInterface :: !(Maybe Int)}
                     deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NetworkInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'niIPv6Supported' - Indicates whether IPv6 is supported.
--
-- * 'niEnaSupport' - Indicates whether Elastic Network Adapter (ENA) is supported.
--
-- * 'niMaximumNetworkInterfaces' - The maximum number of network interfaces for the instance type.
--
-- * 'niIPv6AddressesPerInterface' - The maximum number of IPv6 addresses per network interface.
--
-- * 'niNetworkPerformance' - Describes the network performance.
--
-- * 'niIPv4AddressesPerInterface' - The maximum number of IPv4 addresses per network interface.
networkInfo
    :: NetworkInfo
networkInfo
  = NetworkInfo'{_niIPv6Supported = Nothing,
                 _niEnaSupport = Nothing,
                 _niMaximumNetworkInterfaces = Nothing,
                 _niIPv6AddressesPerInterface = Nothing,
                 _niNetworkPerformance = Nothing,
                 _niIPv4AddressesPerInterface = Nothing}

-- | Indicates whether IPv6 is supported.
niIPv6Supported :: Lens' NetworkInfo (Maybe Bool)
niIPv6Supported = lens _niIPv6Supported (\ s a -> s{_niIPv6Supported = a})

-- | Indicates whether Elastic Network Adapter (ENA) is supported.
niEnaSupport :: Lens' NetworkInfo (Maybe EnaSupport)
niEnaSupport = lens _niEnaSupport (\ s a -> s{_niEnaSupport = a})

-- | The maximum number of network interfaces for the instance type.
niMaximumNetworkInterfaces :: Lens' NetworkInfo (Maybe Int)
niMaximumNetworkInterfaces = lens _niMaximumNetworkInterfaces (\ s a -> s{_niMaximumNetworkInterfaces = a})

-- | The maximum number of IPv6 addresses per network interface.
niIPv6AddressesPerInterface :: Lens' NetworkInfo (Maybe Int)
niIPv6AddressesPerInterface = lens _niIPv6AddressesPerInterface (\ s a -> s{_niIPv6AddressesPerInterface = a})

-- | Describes the network performance.
niNetworkPerformance :: Lens' NetworkInfo (Maybe Text)
niNetworkPerformance = lens _niNetworkPerformance (\ s a -> s{_niNetworkPerformance = a})

-- | The maximum number of IPv4 addresses per network interface.
niIPv4AddressesPerInterface :: Lens' NetworkInfo (Maybe Int)
niIPv4AddressesPerInterface = lens _niIPv4AddressesPerInterface (\ s a -> s{_niIPv4AddressesPerInterface = a})

instance FromXML NetworkInfo where
        parseXML x
          = NetworkInfo' <$>
              (x .@? "ipv6Supported") <*> (x .@? "enaSupport") <*>
                (x .@? "maximumNetworkInterfaces")
                <*> (x .@? "ipv6AddressesPerInterface")
                <*> (x .@? "networkPerformance")
                <*> (x .@? "ipv4AddressesPerInterface")

instance Hashable NetworkInfo where

instance NFData NetworkInfo where
