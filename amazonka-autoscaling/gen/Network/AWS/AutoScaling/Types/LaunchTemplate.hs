{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.LaunchTemplate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.LaunchTemplate where

import Network.AWS.AutoScaling.Types.LaunchTemplateOverrides
import Network.AWS.AutoScaling.Types.LaunchTemplateSpecification
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a launch template and overrides.
--
--
-- The overrides are used to override the instance type specified by the launch template with multiple instance types that can be used to launch On-Demand Instances and Spot Instances.
--
-- When you update the launch template or overrides, existing Amazon EC2 instances continue to run. When scale out occurs, Amazon EC2 Auto Scaling launches instances to match the new settings. When scale in occurs, Amazon EC2 Auto Scaling terminates instances according to the group's termination policies.
--
--
-- /See:/ 'launchTemplate' smart constructor.
data LaunchTemplate = LaunchTemplate'{_ltOverrides ::
                                      !(Maybe [LaunchTemplateOverrides]),
                                      _ltLaunchTemplateSpecification ::
                                      !(Maybe LaunchTemplateSpecification)}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LaunchTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltOverrides' - An optional setting. Any parameters that you specify override the same parameters in the launch template. Currently, the only supported override is instance type. You can specify between 1 and 20 instance types.
--
-- * 'ltLaunchTemplateSpecification' - The launch template to use. You must specify either the launch template ID or launch template name in the request.
launchTemplate
    :: LaunchTemplate
launchTemplate
  = LaunchTemplate'{_ltOverrides = Nothing,
                    _ltLaunchTemplateSpecification = Nothing}

-- | An optional setting. Any parameters that you specify override the same parameters in the launch template. Currently, the only supported override is instance type. You can specify between 1 and 20 instance types.
ltOverrides :: Lens' LaunchTemplate [LaunchTemplateOverrides]
ltOverrides = lens _ltOverrides (\ s a -> s{_ltOverrides = a}) . _Default . _Coerce

-- | The launch template to use. You must specify either the launch template ID or launch template name in the request.
ltLaunchTemplateSpecification :: Lens' LaunchTemplate (Maybe LaunchTemplateSpecification)
ltLaunchTemplateSpecification = lens _ltLaunchTemplateSpecification (\ s a -> s{_ltLaunchTemplateSpecification = a})

instance FromXML LaunchTemplate where
        parseXML x
          = LaunchTemplate' <$>
              (x .@? "Overrides" .!@ mempty >>=
                 may (parseXMLList "member"))
                <*> (x .@? "LaunchTemplateSpecification")

instance Hashable LaunchTemplate where

instance NFData LaunchTemplate where

instance ToQuery LaunchTemplate where
        toQuery LaunchTemplate'{..}
          = mconcat
              ["Overrides" =:
                 toQuery (toQueryList "member" <$> _ltOverrides),
               "LaunchTemplateSpecification" =:
                 _ltLaunchTemplateSpecification]
