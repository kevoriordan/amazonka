{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.LinuxParameters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Batch.Types.LinuxParameters where

import Network.AWS.Batch.Types.Device
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Linux-specific modifications that are applied to the container, such as details for device mappings.
--
--
--
-- /See:/ 'linuxParameters' smart constructor.
newtype LinuxParameters = LinuxParameters'{_lpDevices
                                           :: Maybe [Device]}
                            deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LinuxParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpDevices' - Any host devices to expose to the container. This parameter maps to @Devices@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--device@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
linuxParameters
    :: LinuxParameters
linuxParameters
  = LinuxParameters'{_lpDevices = Nothing}

-- | Any host devices to expose to the container. This parameter maps to @Devices@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--device@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
lpDevices :: Lens' LinuxParameters [Device]
lpDevices = lens _lpDevices (\ s a -> s{_lpDevices = a}) . _Default . _Coerce

instance FromJSON LinuxParameters where
        parseJSON
          = withObject "LinuxParameters"
              (\ x ->
                 LinuxParameters' <$> (x .:? "devices" .!= mempty))

instance Hashable LinuxParameters where

instance NFData LinuxParameters where

instance ToJSON LinuxParameters where
        toJSON LinuxParameters'{..}
          = object (catMaybes [("devices" .=) <$> _lpDevices])
