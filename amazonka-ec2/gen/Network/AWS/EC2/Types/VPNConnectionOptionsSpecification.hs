{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPNConnectionOptionsSpecification
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VPNConnectionOptionsSpecification where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.VPNTunnelOptionsSpecification
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes VPN connection options.
--
--
--
-- /See:/ 'vpnConnectionOptionsSpecification' smart constructor.
data VPNConnectionOptionsSpecification = VPNConnectionOptionsSpecification'{_vcosEnableAcceleration
                                                                            ::
                                                                            !(Maybe
                                                                                Bool),
                                                                            _vcosTunnelOptions
                                                                            ::
                                                                            !(Maybe
                                                                                [VPNTunnelOptionsSpecification]),
                                                                            _vcosStaticRoutesOnly
                                                                            ::
                                                                            !(Maybe
                                                                                Bool)}
                                           deriving (Eq, Read, Show, Data,
                                                     Typeable, Generic)

-- | Creates a value of 'VPNConnectionOptionsSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcosEnableAcceleration' - Indicate whether to enable acceleration for the VPN connection. Default: @false@ 
--
-- * 'vcosTunnelOptions' - The tunnel options for the VPN connection.
--
-- * 'vcosStaticRoutesOnly' - Indicate whether the VPN connection uses static routes only. If you are creating a VPN connection for a device that does not support BGP, you must specify @true@ . Use 'CreateVpnConnectionRoute' to create a static route. Default: @false@ 
vpnConnectionOptionsSpecification
    :: VPNConnectionOptionsSpecification
vpnConnectionOptionsSpecification
  = VPNConnectionOptionsSpecification'{_vcosEnableAcceleration
                                         = Nothing,
                                       _vcosTunnelOptions = Nothing,
                                       _vcosStaticRoutesOnly = Nothing}

-- | Indicate whether to enable acceleration for the VPN connection. Default: @false@ 
vcosEnableAcceleration :: Lens' VPNConnectionOptionsSpecification (Maybe Bool)
vcosEnableAcceleration = lens _vcosEnableAcceleration (\ s a -> s{_vcosEnableAcceleration = a})

-- | The tunnel options for the VPN connection.
vcosTunnelOptions :: Lens' VPNConnectionOptionsSpecification [VPNTunnelOptionsSpecification]
vcosTunnelOptions = lens _vcosTunnelOptions (\ s a -> s{_vcosTunnelOptions = a}) . _Default . _Coerce

-- | Indicate whether the VPN connection uses static routes only. If you are creating a VPN connection for a device that does not support BGP, you must specify @true@ . Use 'CreateVpnConnectionRoute' to create a static route. Default: @false@ 
vcosStaticRoutesOnly :: Lens' VPNConnectionOptionsSpecification (Maybe Bool)
vcosStaticRoutesOnly = lens _vcosStaticRoutesOnly (\ s a -> s{_vcosStaticRoutesOnly = a})

instance Hashable VPNConnectionOptionsSpecification
         where

instance NFData VPNConnectionOptionsSpecification
         where

instance ToQuery VPNConnectionOptionsSpecification
         where
        toQuery VPNConnectionOptionsSpecification'{..}
          = mconcat
              ["EnableAcceleration" =: _vcosEnableAcceleration,
               toQuery
                 (toQueryList "TunnelOptions" <$> _vcosTunnelOptions),
               "StaticRoutesOnly" =: _vcosStaticRoutesOnly]
