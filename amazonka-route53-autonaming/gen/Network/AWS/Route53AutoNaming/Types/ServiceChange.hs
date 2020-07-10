{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.ServiceChange
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53AutoNaming.Types.ServiceChange where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53AutoNaming.Types.DNSConfigChange
import Network.AWS.Route53AutoNaming.Types.HealthCheckConfig

-- | A complex type that contains changes to an existing service.
--
--
--
-- /See:/ 'serviceChange' smart constructor.
data ServiceChange = ServiceChange'{_scHealthCheckConfig
                                    :: !(Maybe HealthCheckConfig),
                                    _scDescription :: !(Maybe Text),
                                    _scDNSConfig :: !DNSConfigChange}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServiceChange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scHealthCheckConfig' - Undocumented member.
--
-- * 'scDescription' - A description for the service.
--
-- * 'scDNSConfig' - A complex type that contains information about the Route 53 DNS records that you want AWS Cloud Map to create when you register an instance.
serviceChange
    :: DNSConfigChange -- ^ 'scDNSConfig'
    -> ServiceChange
serviceChange pDNSConfig_
  = ServiceChange'{_scHealthCheckConfig = Nothing,
                   _scDescription = Nothing, _scDNSConfig = pDNSConfig_}

-- | Undocumented member.
scHealthCheckConfig :: Lens' ServiceChange (Maybe HealthCheckConfig)
scHealthCheckConfig = lens _scHealthCheckConfig (\ s a -> s{_scHealthCheckConfig = a})

-- | A description for the service.
scDescription :: Lens' ServiceChange (Maybe Text)
scDescription = lens _scDescription (\ s a -> s{_scDescription = a})

-- | A complex type that contains information about the Route 53 DNS records that you want AWS Cloud Map to create when you register an instance.
scDNSConfig :: Lens' ServiceChange DNSConfigChange
scDNSConfig = lens _scDNSConfig (\ s a -> s{_scDNSConfig = a})

instance Hashable ServiceChange where

instance NFData ServiceChange where

instance ToJSON ServiceChange where
        toJSON ServiceChange'{..}
          = object
              (catMaybes
                 [("HealthCheckConfig" .=) <$> _scHealthCheckConfig,
                  ("Description" .=) <$> _scDescription,
                  Just ("DnsConfig" .= _scDNSConfig)])
