{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.LaunchTemplateSpecification
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.LaunchTemplateSpecification where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a launch template and the launch template version.
--
--
-- The launch template that is specified must be configured for use with an Auto Scaling group. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-template.html Creating a Launch Template for an Auto Scaling Group> in the /Amazon EC2 Auto Scaling User Guide/ .
--
--
-- /See:/ 'launchTemplateSpecification' smart constructor.
data LaunchTemplateSpecification = LaunchTemplateSpecification'{_ltsLaunchTemplateName
                                                                ::
                                                                !(Maybe Text),
                                                                _ltsLaunchTemplateId
                                                                ::
                                                                !(Maybe Text),
                                                                _ltsVersion ::
                                                                !(Maybe Text)}
                                     deriving (Eq, Read, Show, Data, Typeable,
                                               Generic)

-- | Creates a value of 'LaunchTemplateSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltsLaunchTemplateName' - The name of the launch template. You must specify either a template name or a template ID.
--
-- * 'ltsLaunchTemplateId' - The ID of the launch template. You must specify either a template ID or a template name.
--
-- * 'ltsVersion' - The version number, @> Latest@ , or @> Default@ . If the value is @> Latest@ , Amazon EC2 Auto Scaling selects the latest version of the launch template when launching instances. If the value is @> Default@ , Amazon EC2 Auto Scaling selects the default version of the launch template when launching instances. The default value is @> Default@ .
launchTemplateSpecification
    :: LaunchTemplateSpecification
launchTemplateSpecification
  = LaunchTemplateSpecification'{_ltsLaunchTemplateName
                                   = Nothing,
                                 _ltsLaunchTemplateId = Nothing,
                                 _ltsVersion = Nothing}

-- | The name of the launch template. You must specify either a template name or a template ID.
ltsLaunchTemplateName :: Lens' LaunchTemplateSpecification (Maybe Text)
ltsLaunchTemplateName = lens _ltsLaunchTemplateName (\ s a -> s{_ltsLaunchTemplateName = a})

-- | The ID of the launch template. You must specify either a template ID or a template name.
ltsLaunchTemplateId :: Lens' LaunchTemplateSpecification (Maybe Text)
ltsLaunchTemplateId = lens _ltsLaunchTemplateId (\ s a -> s{_ltsLaunchTemplateId = a})

-- | The version number, @> Latest@ , or @> Default@ . If the value is @> Latest@ , Amazon EC2 Auto Scaling selects the latest version of the launch template when launching instances. If the value is @> Default@ , Amazon EC2 Auto Scaling selects the default version of the launch template when launching instances. The default value is @> Default@ .
ltsVersion :: Lens' LaunchTemplateSpecification (Maybe Text)
ltsVersion = lens _ltsVersion (\ s a -> s{_ltsVersion = a})

instance FromXML LaunchTemplateSpecification where
        parseXML x
          = LaunchTemplateSpecification' <$>
              (x .@? "LaunchTemplateName") <*>
                (x .@? "LaunchTemplateId")
                <*> (x .@? "Version")

instance Hashable LaunchTemplateSpecification where

instance NFData LaunchTemplateSpecification where

instance ToQuery LaunchTemplateSpecification where
        toQuery LaunchTemplateSpecification'{..}
          = mconcat
              ["LaunchTemplateName" =: _ltsLaunchTemplateName,
               "LaunchTemplateId" =: _ltsLaunchTemplateId,
               "Version" =: _ltsVersion]
