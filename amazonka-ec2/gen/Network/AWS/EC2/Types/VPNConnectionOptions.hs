{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPNConnectionOptions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VPNConnectionOptions where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.TunnelOption
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes VPN connection options.
--
--
--
-- /See:/ 'vpnConnectionOptions' smart constructor.
data VPNConnectionOptions = VPNConnectionOptions'{_vcoEnableAcceleration
                                                  :: !(Maybe Bool),
                                                  _vcoTunnelOptions ::
                                                  !(Maybe [TunnelOption]),
                                                  _vcoStaticRoutesOnly ::
                                                  !(Maybe Bool)}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VPNConnectionOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcoEnableAcceleration' - Indicates whether acceleration is enabled for the VPN connection.
--
-- * 'vcoTunnelOptions' - Indicates the VPN tunnel options.
--
-- * 'vcoStaticRoutesOnly' - Indicates whether the VPN connection uses static routes only. Static routes must be used for devices that don't support BGP.
vpnConnectionOptions
    :: VPNConnectionOptions
vpnConnectionOptions
  = VPNConnectionOptions'{_vcoEnableAcceleration =
                            Nothing,
                          _vcoTunnelOptions = Nothing,
                          _vcoStaticRoutesOnly = Nothing}

-- | Indicates whether acceleration is enabled for the VPN connection.
vcoEnableAcceleration :: Lens' VPNConnectionOptions (Maybe Bool)
vcoEnableAcceleration = lens _vcoEnableAcceleration (\ s a -> s{_vcoEnableAcceleration = a})

-- | Indicates the VPN tunnel options.
vcoTunnelOptions :: Lens' VPNConnectionOptions [TunnelOption]
vcoTunnelOptions = lens _vcoTunnelOptions (\ s a -> s{_vcoTunnelOptions = a}) . _Default . _Coerce

-- | Indicates whether the VPN connection uses static routes only. Static routes must be used for devices that don't support BGP.
vcoStaticRoutesOnly :: Lens' VPNConnectionOptions (Maybe Bool)
vcoStaticRoutesOnly = lens _vcoStaticRoutesOnly (\ s a -> s{_vcoStaticRoutesOnly = a})

instance FromXML VPNConnectionOptions where
        parseXML x
          = VPNConnectionOptions' <$>
              (x .@? "enableAcceleration") <*>
                (x .@? "tunnelOptionSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "staticRoutesOnly")

instance Hashable VPNConnectionOptions where

instance NFData VPNConnectionOptions where
