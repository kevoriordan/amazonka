{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceProperties
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.WorkspaceProperties where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkSpaces.Types.Compute
import Network.AWS.WorkSpaces.Types.RunningMode

-- | Describes a WorkSpace.
--
--
--
-- /See:/ 'workspaceProperties' smart constructor.
data WorkspaceProperties = WorkspaceProperties'{_wpComputeTypeName
                                                :: !(Maybe Compute),
                                                _wpRunningMode ::
                                                !(Maybe RunningMode),
                                                _wpRootVolumeSizeGib ::
                                                !(Maybe Int),
                                                _wpRunningModeAutoStopTimeoutInMinutes
                                                :: !(Maybe Int),
                                                _wpUserVolumeSizeGib ::
                                                !(Maybe Int)}
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WorkspaceProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wpComputeTypeName' - The compute type. For more information, see <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles> .
--
-- * 'wpRunningMode' - The running mode. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/running-mode.html Manage the WorkSpace Running Mode> .
--
-- * 'wpRootVolumeSizeGib' - The size of the root volume.
--
-- * 'wpRunningModeAutoStopTimeoutInMinutes' - The time after a user logs off when WorkSpaces are automatically stopped. Configured in 60-minute intervals.
--
-- * 'wpUserVolumeSizeGib' - The size of the user storage.
workspaceProperties
    :: WorkspaceProperties
workspaceProperties
  = WorkspaceProperties'{_wpComputeTypeName = Nothing,
                         _wpRunningMode = Nothing,
                         _wpRootVolumeSizeGib = Nothing,
                         _wpRunningModeAutoStopTimeoutInMinutes = Nothing,
                         _wpUserVolumeSizeGib = Nothing}

-- | The compute type. For more information, see <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles> .
wpComputeTypeName :: Lens' WorkspaceProperties (Maybe Compute)
wpComputeTypeName = lens _wpComputeTypeName (\ s a -> s{_wpComputeTypeName = a})

-- | The running mode. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/running-mode.html Manage the WorkSpace Running Mode> .
wpRunningMode :: Lens' WorkspaceProperties (Maybe RunningMode)
wpRunningMode = lens _wpRunningMode (\ s a -> s{_wpRunningMode = a})

-- | The size of the root volume.
wpRootVolumeSizeGib :: Lens' WorkspaceProperties (Maybe Int)
wpRootVolumeSizeGib = lens _wpRootVolumeSizeGib (\ s a -> s{_wpRootVolumeSizeGib = a})

-- | The time after a user logs off when WorkSpaces are automatically stopped. Configured in 60-minute intervals.
wpRunningModeAutoStopTimeoutInMinutes :: Lens' WorkspaceProperties (Maybe Int)
wpRunningModeAutoStopTimeoutInMinutes = lens _wpRunningModeAutoStopTimeoutInMinutes (\ s a -> s{_wpRunningModeAutoStopTimeoutInMinutes = a})

-- | The size of the user storage.
wpUserVolumeSizeGib :: Lens' WorkspaceProperties (Maybe Int)
wpUserVolumeSizeGib = lens _wpUserVolumeSizeGib (\ s a -> s{_wpUserVolumeSizeGib = a})

instance FromJSON WorkspaceProperties where
        parseJSON
          = withObject "WorkspaceProperties"
              (\ x ->
                 WorkspaceProperties' <$>
                   (x .:? "ComputeTypeName") <*> (x .:? "RunningMode")
                     <*> (x .:? "RootVolumeSizeGib")
                     <*> (x .:? "RunningModeAutoStopTimeoutInMinutes")
                     <*> (x .:? "UserVolumeSizeGib"))

instance Hashable WorkspaceProperties where

instance NFData WorkspaceProperties where

instance ToJSON WorkspaceProperties where
        toJSON WorkspaceProperties'{..}
          = object
              (catMaybes
                 [("ComputeTypeName" .=) <$> _wpComputeTypeName,
                  ("RunningMode" .=) <$> _wpRunningMode,
                  ("RootVolumeSizeGib" .=) <$> _wpRootVolumeSizeGib,
                  ("RunningModeAutoStopTimeoutInMinutes" .=) <$>
                    _wpRunningModeAutoStopTimeoutInMinutes,
                  ("UserVolumeSizeGib" .=) <$> _wpUserVolumeSizeGib])
