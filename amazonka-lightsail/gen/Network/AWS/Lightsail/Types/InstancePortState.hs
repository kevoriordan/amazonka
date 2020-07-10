{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstancePortState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.InstancePortState where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.NetworkProtocol
import Network.AWS.Lightsail.Types.PortState
import Network.AWS.Prelude

-- | Describes the port state.
--
--
--
-- /See:/ 'instancePortState' smart constructor.
data InstancePortState = InstancePortState'{_ipsFromPort
                                            :: !(Maybe Nat),
                                            _ipsState :: !(Maybe PortState),
                                            _ipsProtocol ::
                                            !(Maybe NetworkProtocol),
                                            _ipsToPort :: !(Maybe Nat)}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstancePortState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipsFromPort' - The first port in the range.
--
-- * 'ipsState' - Specifies whether the instance port is @open@ or @closed@ .
--
-- * 'ipsProtocol' - The protocol being used. Can be one of the following.     * @tcp@ - Transmission Control Protocol (TCP) provides reliable, ordered, and error-checked delivery of streamed data between applications running on hosts communicating by an IP network. If you have an application that doesn't require reliable data stream service, use UDP instead.     * @all@ - All transport layer protocol types. For more general information, see <https://en.wikipedia.org/wiki/Transport_layer Transport layer> on Wikipedia.     * @udp@ - With User Datagram Protocol (UDP), computer applications can send messages (or datagrams) to other hosts on an Internet Protocol (IP) network. Prior communications are not required to set up transmission channels or data paths. Applications that don't require reliable data stream service can use UDP, which provides a connectionless datagram service that emphasizes reduced latency over reliability. If you do require reliable data stream service, use TCP instead.
--
-- * 'ipsToPort' - The last port in the range.
instancePortState
    :: InstancePortState
instancePortState
  = InstancePortState'{_ipsFromPort = Nothing,
                       _ipsState = Nothing, _ipsProtocol = Nothing,
                       _ipsToPort = Nothing}

-- | The first port in the range.
ipsFromPort :: Lens' InstancePortState (Maybe Natural)
ipsFromPort = lens _ipsFromPort (\ s a -> s{_ipsFromPort = a}) . mapping _Nat

-- | Specifies whether the instance port is @open@ or @closed@ .
ipsState :: Lens' InstancePortState (Maybe PortState)
ipsState = lens _ipsState (\ s a -> s{_ipsState = a})

-- | The protocol being used. Can be one of the following.     * @tcp@ - Transmission Control Protocol (TCP) provides reliable, ordered, and error-checked delivery of streamed data between applications running on hosts communicating by an IP network. If you have an application that doesn't require reliable data stream service, use UDP instead.     * @all@ - All transport layer protocol types. For more general information, see <https://en.wikipedia.org/wiki/Transport_layer Transport layer> on Wikipedia.     * @udp@ - With User Datagram Protocol (UDP), computer applications can send messages (or datagrams) to other hosts on an Internet Protocol (IP) network. Prior communications are not required to set up transmission channels or data paths. Applications that don't require reliable data stream service can use UDP, which provides a connectionless datagram service that emphasizes reduced latency over reliability. If you do require reliable data stream service, use TCP instead.
ipsProtocol :: Lens' InstancePortState (Maybe NetworkProtocol)
ipsProtocol = lens _ipsProtocol (\ s a -> s{_ipsProtocol = a})

-- | The last port in the range.
ipsToPort :: Lens' InstancePortState (Maybe Natural)
ipsToPort = lens _ipsToPort (\ s a -> s{_ipsToPort = a}) . mapping _Nat

instance FromJSON InstancePortState where
        parseJSON
          = withObject "InstancePortState"
              (\ x ->
                 InstancePortState' <$>
                   (x .:? "fromPort") <*> (x .:? "state") <*>
                     (x .:? "protocol")
                     <*> (x .:? "toPort"))

instance Hashable InstancePortState where

instance NFData InstancePortState where
