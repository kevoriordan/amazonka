{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstancePortInfo
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.InstancePortInfo where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.AccessDirection
import Network.AWS.Lightsail.Types.NetworkProtocol
import Network.AWS.Lightsail.Types.PortAccessType
import Network.AWS.Prelude

-- | Describes information about the instance ports.
--
--
--
-- /See:/ 'instancePortInfo' smart constructor.
data InstancePortInfo = InstancePortInfo'{_ipiFromPort
                                          :: !(Maybe Nat),
                                          _ipiCommonName :: !(Maybe Text),
                                          _ipiProtocol ::
                                          !(Maybe NetworkProtocol),
                                          _ipiAccessDirection ::
                                          !(Maybe AccessDirection),
                                          _ipiAccessType ::
                                          !(Maybe PortAccessType),
                                          _ipiToPort :: !(Maybe Nat),
                                          _ipiAccessFrom :: !(Maybe Text)}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstancePortInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipiFromPort' - The first port in the range.
--
-- * 'ipiCommonName' - The common name.
--
-- * 'ipiProtocol' - The protocol being used. Can be one of the following.     * @tcp@ - Transmission Control Protocol (TCP) provides reliable, ordered, and error-checked delivery of streamed data between applications running on hosts communicating by an IP network. If you have an application that doesn't require reliable data stream service, use UDP instead.     * @all@ - All transport layer protocol types. For more general information, see <https://en.wikipedia.org/wiki/Transport_layer Transport layer> on Wikipedia.     * @udp@ - With User Datagram Protocol (UDP), computer applications can send messages (or datagrams) to other hosts on an Internet Protocol (IP) network. Prior communications are not required to set up transmission channels or data paths. Applications that don't require reliable data stream service can use UDP, which provides a connectionless datagram service that emphasizes reduced latency over reliability. If you do require reliable data stream service, use TCP instead.
--
-- * 'ipiAccessDirection' - The access direction (@inbound@ or @outbound@ ).
--
-- * 'ipiAccessType' - The type of access (@Public@ or @Private@ ).
--
-- * 'ipiToPort' - The last port in the range.
--
-- * 'ipiAccessFrom' - The location from which access is allowed (e.g., @Anywhere (0.0.0.0/0)@ ).
instancePortInfo
    :: InstancePortInfo
instancePortInfo
  = InstancePortInfo'{_ipiFromPort = Nothing,
                      _ipiCommonName = Nothing, _ipiProtocol = Nothing,
                      _ipiAccessDirection = Nothing,
                      _ipiAccessType = Nothing, _ipiToPort = Nothing,
                      _ipiAccessFrom = Nothing}

-- | The first port in the range.
ipiFromPort :: Lens' InstancePortInfo (Maybe Natural)
ipiFromPort = lens _ipiFromPort (\ s a -> s{_ipiFromPort = a}) . mapping _Nat

-- | The common name.
ipiCommonName :: Lens' InstancePortInfo (Maybe Text)
ipiCommonName = lens _ipiCommonName (\ s a -> s{_ipiCommonName = a})

-- | The protocol being used. Can be one of the following.     * @tcp@ - Transmission Control Protocol (TCP) provides reliable, ordered, and error-checked delivery of streamed data between applications running on hosts communicating by an IP network. If you have an application that doesn't require reliable data stream service, use UDP instead.     * @all@ - All transport layer protocol types. For more general information, see <https://en.wikipedia.org/wiki/Transport_layer Transport layer> on Wikipedia.     * @udp@ - With User Datagram Protocol (UDP), computer applications can send messages (or datagrams) to other hosts on an Internet Protocol (IP) network. Prior communications are not required to set up transmission channels or data paths. Applications that don't require reliable data stream service can use UDP, which provides a connectionless datagram service that emphasizes reduced latency over reliability. If you do require reliable data stream service, use TCP instead.
ipiProtocol :: Lens' InstancePortInfo (Maybe NetworkProtocol)
ipiProtocol = lens _ipiProtocol (\ s a -> s{_ipiProtocol = a})

-- | The access direction (@inbound@ or @outbound@ ).
ipiAccessDirection :: Lens' InstancePortInfo (Maybe AccessDirection)
ipiAccessDirection = lens _ipiAccessDirection (\ s a -> s{_ipiAccessDirection = a})

-- | The type of access (@Public@ or @Private@ ).
ipiAccessType :: Lens' InstancePortInfo (Maybe PortAccessType)
ipiAccessType = lens _ipiAccessType (\ s a -> s{_ipiAccessType = a})

-- | The last port in the range.
ipiToPort :: Lens' InstancePortInfo (Maybe Natural)
ipiToPort = lens _ipiToPort (\ s a -> s{_ipiToPort = a}) . mapping _Nat

-- | The location from which access is allowed (e.g., @Anywhere (0.0.0.0/0)@ ).
ipiAccessFrom :: Lens' InstancePortInfo (Maybe Text)
ipiAccessFrom = lens _ipiAccessFrom (\ s a -> s{_ipiAccessFrom = a})

instance FromJSON InstancePortInfo where
        parseJSON
          = withObject "InstancePortInfo"
              (\ x ->
                 InstancePortInfo' <$>
                   (x .:? "fromPort") <*> (x .:? "commonName") <*>
                     (x .:? "protocol")
                     <*> (x .:? "accessDirection")
                     <*> (x .:? "accessType")
                     <*> (x .:? "toPort")
                     <*> (x .:? "accessFrom"))

instance Hashable InstancePortInfo where

instance NFData InstancePortInfo where
