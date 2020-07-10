{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NetworkConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.NetworkConfig where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.VPCConfig

-- | Networking options for a job, such as network traffic encryption between containers, whether to allow inbound and outbound network calls to and from containers, and the VPC subnets and security groups to use for VPC-enabled jobs.
--
--
--
-- /See:/ 'networkConfig' smart constructor.
data NetworkConfig = NetworkConfig'{_ncEnableNetworkIsolation
                                    :: !(Maybe Bool),
                                    _ncVPCConfig :: !(Maybe VPCConfig)}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NetworkConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncEnableNetworkIsolation' - Whether to allow inbound and outbound network calls to and from the containers used for the processing job.
--
-- * 'ncVPCConfig' - Undocumented member.
networkConfig
    :: NetworkConfig
networkConfig
  = NetworkConfig'{_ncEnableNetworkIsolation = Nothing,
                   _ncVPCConfig = Nothing}

-- | Whether to allow inbound and outbound network calls to and from the containers used for the processing job.
ncEnableNetworkIsolation :: Lens' NetworkConfig (Maybe Bool)
ncEnableNetworkIsolation = lens _ncEnableNetworkIsolation (\ s a -> s{_ncEnableNetworkIsolation = a})

-- | Undocumented member.
ncVPCConfig :: Lens' NetworkConfig (Maybe VPCConfig)
ncVPCConfig = lens _ncVPCConfig (\ s a -> s{_ncVPCConfig = a})

instance FromJSON NetworkConfig where
        parseJSON
          = withObject "NetworkConfig"
              (\ x ->
                 NetworkConfig' <$>
                   (x .:? "EnableNetworkIsolation") <*>
                     (x .:? "VpcConfig"))

instance Hashable NetworkConfig where

instance NFData NetworkConfig where

instance ToJSON NetworkConfig where
        toJSON NetworkConfig'{..}
          = object
              (catMaybes
                 [("EnableNetworkIsolation" .=) <$>
                    _ncEnableNetworkIsolation,
                  ("VpcConfig" .=) <$> _ncVPCConfig])
