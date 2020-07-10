{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetLaunchTemplateSpecification
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.FleetLaunchTemplateSpecification where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a launch template.
--
--
--
-- /See:/ 'fleetLaunchTemplateSpecification' smart constructor.
data FleetLaunchTemplateSpecification = FleetLaunchTemplateSpecification'{_fltsLaunchTemplateName
                                                                          ::
                                                                          !(Maybe
                                                                              Text),
                                                                          _fltsLaunchTemplateId
                                                                          ::
                                                                          !(Maybe
                                                                              Text),
                                                                          _fltsVersion
                                                                          ::
                                                                          !(Maybe
                                                                              Text)}
                                          deriving (Eq, Read, Show, Data,
                                                    Typeable, Generic)

-- | Creates a value of 'FleetLaunchTemplateSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fltsLaunchTemplateName' - The name of the launch template. You must specify either a template name or a template ID.
--
-- * 'fltsLaunchTemplateId' - The ID of the launch template. You must specify either a template ID or a template name.
--
-- * 'fltsVersion' - The version number of the launch template. You must specify a version number.
fleetLaunchTemplateSpecification
    :: FleetLaunchTemplateSpecification
fleetLaunchTemplateSpecification
  = FleetLaunchTemplateSpecification'{_fltsLaunchTemplateName
                                        = Nothing,
                                      _fltsLaunchTemplateId = Nothing,
                                      _fltsVersion = Nothing}

-- | The name of the launch template. You must specify either a template name or a template ID.
fltsLaunchTemplateName :: Lens' FleetLaunchTemplateSpecification (Maybe Text)
fltsLaunchTemplateName = lens _fltsLaunchTemplateName (\ s a -> s{_fltsLaunchTemplateName = a})

-- | The ID of the launch template. You must specify either a template ID or a template name.
fltsLaunchTemplateId :: Lens' FleetLaunchTemplateSpecification (Maybe Text)
fltsLaunchTemplateId = lens _fltsLaunchTemplateId (\ s a -> s{_fltsLaunchTemplateId = a})

-- | The version number of the launch template. You must specify a version number.
fltsVersion :: Lens' FleetLaunchTemplateSpecification (Maybe Text)
fltsVersion = lens _fltsVersion (\ s a -> s{_fltsVersion = a})

instance FromXML FleetLaunchTemplateSpecification
         where
        parseXML x
          = FleetLaunchTemplateSpecification' <$>
              (x .@? "launchTemplateName") <*>
                (x .@? "launchTemplateId")
                <*> (x .@? "version")

instance Hashable FleetLaunchTemplateSpecification
         where

instance NFData FleetLaunchTemplateSpecification
         where

instance ToQuery FleetLaunchTemplateSpecification
         where
        toQuery FleetLaunchTemplateSpecification'{..}
          = mconcat
              ["LaunchTemplateName" =: _fltsLaunchTemplateName,
               "LaunchTemplateId" =: _fltsLaunchTemplateId,
               "Version" =: _fltsVersion]
