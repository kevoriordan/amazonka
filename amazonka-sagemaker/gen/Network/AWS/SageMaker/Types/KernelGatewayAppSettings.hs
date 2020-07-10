{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.KernelGatewayAppSettings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.KernelGatewayAppSettings where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.ResourceSpec

-- | The kernel gateway app settings.
--
--
--
-- /See:/ 'kernelGatewayAppSettings' smart constructor.
newtype KernelGatewayAppSettings = KernelGatewayAppSettings'{_kgasDefaultResourceSpec
                                                             ::
                                                             Maybe ResourceSpec}
                                     deriving (Eq, Read, Show, Data, Typeable,
                                               Generic)

-- | Creates a value of 'KernelGatewayAppSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kgasDefaultResourceSpec' - The instance type and quantity.
kernelGatewayAppSettings
    :: KernelGatewayAppSettings
kernelGatewayAppSettings
  = KernelGatewayAppSettings'{_kgasDefaultResourceSpec
                                = Nothing}

-- | The instance type and quantity.
kgasDefaultResourceSpec :: Lens' KernelGatewayAppSettings (Maybe ResourceSpec)
kgasDefaultResourceSpec = lens _kgasDefaultResourceSpec (\ s a -> s{_kgasDefaultResourceSpec = a})

instance FromJSON KernelGatewayAppSettings where
        parseJSON
          = withObject "KernelGatewayAppSettings"
              (\ x ->
                 KernelGatewayAppSettings' <$>
                   (x .:? "DefaultResourceSpec"))

instance Hashable KernelGatewayAppSettings where

instance NFData KernelGatewayAppSettings where

instance ToJSON KernelGatewayAppSettings where
        toJSON KernelGatewayAppSettings'{..}
          = object
              (catMaybes
                 [("DefaultResourceSpec" .=) <$>
                    _kgasDefaultResourceSpec])
