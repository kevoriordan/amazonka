{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.PortInfo
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.PortInfo where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.NetworkProtocol
import Network.AWS.Prelude

-- | Describes information about the ports on your virtual private server (or /instance/ ).
--
--
--
-- /See:/ 'portInfo' smart constructor.
data PortInfo = PortInfo'{_piFromPort ::
                          !(Maybe Nat),
                          _piProtocol :: !(Maybe NetworkProtocol),
                          _piToPort :: !(Maybe Nat)}
                  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PortInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piFromPort' - The first port in the range.
--
-- * 'piProtocol' - The protocol. 
--
-- * 'piToPort' - The last port in the range.
portInfo
    :: PortInfo
portInfo
  = PortInfo'{_piFromPort = Nothing,
              _piProtocol = Nothing, _piToPort = Nothing}

-- | The first port in the range.
piFromPort :: Lens' PortInfo (Maybe Natural)
piFromPort = lens _piFromPort (\ s a -> s{_piFromPort = a}) . mapping _Nat

-- | The protocol. 
piProtocol :: Lens' PortInfo (Maybe NetworkProtocol)
piProtocol = lens _piProtocol (\ s a -> s{_piProtocol = a})

-- | The last port in the range.
piToPort :: Lens' PortInfo (Maybe Natural)
piToPort = lens _piToPort (\ s a -> s{_piToPort = a}) . mapping _Nat

instance Hashable PortInfo where

instance NFData PortInfo where

instance ToJSON PortInfo where
        toJSON PortInfo'{..}
          = object
              (catMaybes
                 [("fromPort" .=) <$> _piFromPort,
                  ("protocol" .=) <$> _piProtocol,
                  ("toPort" .=) <$> _piToPort])
