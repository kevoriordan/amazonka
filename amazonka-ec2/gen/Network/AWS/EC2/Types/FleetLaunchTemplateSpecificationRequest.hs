{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetLaunchTemplateSpecificationRequest
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.FleetLaunchTemplateSpecificationRequest where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The launch template to use. You must specify either the launch template ID or launch template name in the request. 
--
--
--
-- /See:/ 'fleetLaunchTemplateSpecificationRequest' smart constructor.
data FleetLaunchTemplateSpecificationRequest = FleetLaunchTemplateSpecificationRequest'{_fltsrLaunchTemplateName
                                                                                        ::
                                                                                        !(Maybe
                                                                                            Text),
                                                                                        _fltsrLaunchTemplateId
                                                                                        ::
                                                                                        !(Maybe
                                                                                            Text),
                                                                                        _fltsrVersion
                                                                                        ::
                                                                                        !(Maybe
                                                                                            Text)}
                                                 deriving (Eq, Read, Show, Data,
                                                           Typeable, Generic)

-- | Creates a value of 'FleetLaunchTemplateSpecificationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fltsrLaunchTemplateName' - The name of the launch template.
--
-- * 'fltsrLaunchTemplateId' - The ID of the launch template.
--
-- * 'fltsrVersion' - The version number of the launch template. Note: This is a required parameter and will be updated soon. 
fleetLaunchTemplateSpecificationRequest
    :: FleetLaunchTemplateSpecificationRequest
fleetLaunchTemplateSpecificationRequest
  = FleetLaunchTemplateSpecificationRequest'{_fltsrLaunchTemplateName
                                               = Nothing,
                                             _fltsrLaunchTemplateId = Nothing,
                                             _fltsrVersion = Nothing}

-- | The name of the launch template.
fltsrLaunchTemplateName :: Lens' FleetLaunchTemplateSpecificationRequest (Maybe Text)
fltsrLaunchTemplateName = lens _fltsrLaunchTemplateName (\ s a -> s{_fltsrLaunchTemplateName = a})

-- | The ID of the launch template.
fltsrLaunchTemplateId :: Lens' FleetLaunchTemplateSpecificationRequest (Maybe Text)
fltsrLaunchTemplateId = lens _fltsrLaunchTemplateId (\ s a -> s{_fltsrLaunchTemplateId = a})

-- | The version number of the launch template. Note: This is a required parameter and will be updated soon. 
fltsrVersion :: Lens' FleetLaunchTemplateSpecificationRequest (Maybe Text)
fltsrVersion = lens _fltsrVersion (\ s a -> s{_fltsrVersion = a})

instance Hashable
           FleetLaunchTemplateSpecificationRequest
         where

instance NFData
           FleetLaunchTemplateSpecificationRequest
         where

instance ToQuery
           FleetLaunchTemplateSpecificationRequest
         where
        toQuery FleetLaunchTemplateSpecificationRequest'{..}
          = mconcat
              ["LaunchTemplateName" =: _fltsrLaunchTemplateName,
               "LaunchTemplateId" =: _fltsrLaunchTemplateId,
               "Version" =: _fltsrVersion]
