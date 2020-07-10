{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.UpdateOrganizationConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the delegated administrator account with the values provided.
--
--
module Network.AWS.GuardDuty.UpdateOrganizationConfiguration
    (
    -- * Creating a Request
      updateOrganizationConfiguration
    , UpdateOrganizationConfiguration
    -- * Request Lenses
    , uocDetectorId
    , uocAutoEnable

    -- * Destructuring the Response
    , updateOrganizationConfigurationResponse
    , UpdateOrganizationConfigurationResponse
    -- * Response Lenses
    , uocrsResponseStatus
    ) where

import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateOrganizationConfiguration' smart constructor.
data UpdateOrganizationConfiguration = UpdateOrganizationConfiguration'{_uocDetectorId
                                                                        ::
                                                                        !Text,
                                                                        _uocAutoEnable
                                                                        ::
                                                                        !Bool}
                                         deriving (Eq, Read, Show, Data,
                                                   Typeable, Generic)

-- | Creates a value of 'UpdateOrganizationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uocDetectorId' - The ID of the detector to update the delegated administrator for.
--
-- * 'uocAutoEnable' - Indicates whether to automatically enable member accounts in the organization.
updateOrganizationConfiguration
    :: Text -- ^ 'uocDetectorId'
    -> Bool -- ^ 'uocAutoEnable'
    -> UpdateOrganizationConfiguration
updateOrganizationConfiguration pDetectorId_
  pAutoEnable_
  = UpdateOrganizationConfiguration'{_uocDetectorId =
                                       pDetectorId_,
                                     _uocAutoEnable = pAutoEnable_}

-- | The ID of the detector to update the delegated administrator for.
uocDetectorId :: Lens' UpdateOrganizationConfiguration Text
uocDetectorId = lens _uocDetectorId (\ s a -> s{_uocDetectorId = a})

-- | Indicates whether to automatically enable member accounts in the organization.
uocAutoEnable :: Lens' UpdateOrganizationConfiguration Bool
uocAutoEnable = lens _uocAutoEnable (\ s a -> s{_uocAutoEnable = a})

instance AWSRequest UpdateOrganizationConfiguration
         where
        type Rs UpdateOrganizationConfiguration =
             UpdateOrganizationConfigurationResponse
        request = postJSON guardDuty
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateOrganizationConfigurationResponse' <$>
                   (pure (fromEnum s)))

instance Hashable UpdateOrganizationConfiguration
         where

instance NFData UpdateOrganizationConfiguration where

instance ToHeaders UpdateOrganizationConfiguration
         where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateOrganizationConfiguration where
        toJSON UpdateOrganizationConfiguration'{..}
          = object
              (catMaybes [Just ("autoEnable" .= _uocAutoEnable)])

instance ToPath UpdateOrganizationConfiguration where
        toPath UpdateOrganizationConfiguration'{..}
          = mconcat
              ["/detector/", toBS _uocDetectorId, "/admin"]

instance ToQuery UpdateOrganizationConfiguration
         where
        toQuery = const mempty

-- | /See:/ 'updateOrganizationConfigurationResponse' smart constructor.
newtype UpdateOrganizationConfigurationResponse = UpdateOrganizationConfigurationResponse'{_uocrsResponseStatus
                                                                                           ::
                                                                                           Int}
                                                    deriving (Eq, Read, Show,
                                                              Data, Typeable,
                                                              Generic)

-- | Creates a value of 'UpdateOrganizationConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uocrsResponseStatus' - -- | The response status code.
updateOrganizationConfigurationResponse
    :: Int -- ^ 'uocrsResponseStatus'
    -> UpdateOrganizationConfigurationResponse
updateOrganizationConfigurationResponse
  pResponseStatus_
  = UpdateOrganizationConfigurationResponse'{_uocrsResponseStatus
                                               = pResponseStatus_}

-- | -- | The response status code.
uocrsResponseStatus :: Lens' UpdateOrganizationConfigurationResponse Int
uocrsResponseStatus = lens _uocrsResponseStatus (\ s a -> s{_uocrsResponseStatus = a})

instance NFData
           UpdateOrganizationConfigurationResponse
         where
