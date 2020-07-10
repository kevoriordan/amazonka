{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceCreationProperties
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.WorkspaceCreationProperties where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the default properties that are used for creating WorkSpaces. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/update-directory-details.html Update Directory Details for Your WorkSpaces> . 
--
--
--
-- /See:/ 'workspaceCreationProperties' smart constructor.
data WorkspaceCreationProperties = WorkspaceCreationProperties'{_wcpCustomSecurityGroupId
                                                                ::
                                                                !(Maybe Text),
                                                                _wcpUserEnabledAsLocalAdministrator
                                                                ::
                                                                !(Maybe Bool),
                                                                _wcpEnableMaintenanceMode
                                                                ::
                                                                !(Maybe Bool),
                                                                _wcpEnableInternetAccess
                                                                ::
                                                                !(Maybe Bool),
                                                                _wcpDefaultOu ::
                                                                !(Maybe Text)}
                                     deriving (Eq, Read, Show, Data, Typeable,
                                               Generic)

-- | Creates a value of 'WorkspaceCreationProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wcpCustomSecurityGroupId' - The identifier of your custom security group.
--
-- * 'wcpUserEnabledAsLocalAdministrator' - Indicates whether users are local administrators of their WorkSpaces.
--
-- * 'wcpEnableMaintenanceMode' - Indicates whether maintenance mode is enabled for your WorkSpaces. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/workspace-maintenance.html WorkSpace Maintenance> . 
--
-- * 'wcpEnableInternetAccess' - Indicates whether internet access is enabled for your WorkSpaces.
--
-- * 'wcpDefaultOu' - The default organizational unit (OU) for your WorkSpace directories.
workspaceCreationProperties
    :: WorkspaceCreationProperties
workspaceCreationProperties
  = WorkspaceCreationProperties'{_wcpCustomSecurityGroupId
                                   = Nothing,
                                 _wcpUserEnabledAsLocalAdministrator = Nothing,
                                 _wcpEnableMaintenanceMode = Nothing,
                                 _wcpEnableInternetAccess = Nothing,
                                 _wcpDefaultOu = Nothing}

-- | The identifier of your custom security group.
wcpCustomSecurityGroupId :: Lens' WorkspaceCreationProperties (Maybe Text)
wcpCustomSecurityGroupId = lens _wcpCustomSecurityGroupId (\ s a -> s{_wcpCustomSecurityGroupId = a})

-- | Indicates whether users are local administrators of their WorkSpaces.
wcpUserEnabledAsLocalAdministrator :: Lens' WorkspaceCreationProperties (Maybe Bool)
wcpUserEnabledAsLocalAdministrator = lens _wcpUserEnabledAsLocalAdministrator (\ s a -> s{_wcpUserEnabledAsLocalAdministrator = a})

-- | Indicates whether maintenance mode is enabled for your WorkSpaces. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/workspace-maintenance.html WorkSpace Maintenance> . 
wcpEnableMaintenanceMode :: Lens' WorkspaceCreationProperties (Maybe Bool)
wcpEnableMaintenanceMode = lens _wcpEnableMaintenanceMode (\ s a -> s{_wcpEnableMaintenanceMode = a})

-- | Indicates whether internet access is enabled for your WorkSpaces.
wcpEnableInternetAccess :: Lens' WorkspaceCreationProperties (Maybe Bool)
wcpEnableInternetAccess = lens _wcpEnableInternetAccess (\ s a -> s{_wcpEnableInternetAccess = a})

-- | The default organizational unit (OU) for your WorkSpace directories.
wcpDefaultOu :: Lens' WorkspaceCreationProperties (Maybe Text)
wcpDefaultOu = lens _wcpDefaultOu (\ s a -> s{_wcpDefaultOu = a})

instance Hashable WorkspaceCreationProperties where

instance NFData WorkspaceCreationProperties where

instance ToJSON WorkspaceCreationProperties where
        toJSON WorkspaceCreationProperties'{..}
          = object
              (catMaybes
                 [("CustomSecurityGroupId" .=) <$>
                    _wcpCustomSecurityGroupId,
                  ("UserEnabledAsLocalAdministrator" .=) <$>
                    _wcpUserEnabledAsLocalAdministrator,
                  ("EnableMaintenanceMode" .=) <$>
                    _wcpEnableMaintenanceMode,
                  ("EnableInternetAccess" .=) <$>
                    _wcpEnableInternetAccess,
                  ("DefaultOu" .=) <$> _wcpDefaultOu])
