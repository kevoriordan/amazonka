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
-- Module      : Network.AWS.AWSHealth.DisableHealthServiceAccessForOrganization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Calling this operation disables Health from working with AWS Organizations. This does not remove the Service Linked Role (SLR) from the the master account in your organization. Use the IAM console, API, or AWS CLI to remove the SLR if desired. To call this operation, you must sign in as an IAM user, assume an IAM role, or sign in as the root user (not recommended) in the organization's master account.
--
--
module Network.AWS.AWSHealth.DisableHealthServiceAccessForOrganization
    (
    -- * Creating a Request
      disableHealthServiceAccessForOrganization
    , DisableHealthServiceAccessForOrganization

    -- * Destructuring the Response
    , disableHealthServiceAccessForOrganizationResponse
    , DisableHealthServiceAccessForOrganizationResponse
    ) where

import Network.AWS.AWSHealth.Types
import Network.AWS.AWSHealth.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disableHealthServiceAccessForOrganization' smart constructor.
data DisableHealthServiceAccessForOrganization = DisableHealthServiceAccessForOrganization'
                                                   deriving (Eq, Read, Show,
                                                             Data, Typeable,
                                                             Generic)

-- | Creates a value of 'DisableHealthServiceAccessForOrganization' with the minimum fields required to make a request.
--
disableHealthServiceAccessForOrganization
    :: DisableHealthServiceAccessForOrganization
disableHealthServiceAccessForOrganization
  = DisableHealthServiceAccessForOrganization'

instance AWSRequest
           DisableHealthServiceAccessForOrganization
         where
        type Rs DisableHealthServiceAccessForOrganization =
             DisableHealthServiceAccessForOrganizationResponse
        request = postJSON awsHealth
        response
          = receiveNull
              DisableHealthServiceAccessForOrganizationResponse'

instance Hashable
           DisableHealthServiceAccessForOrganization
         where

instance NFData
           DisableHealthServiceAccessForOrganization
         where

instance ToHeaders
           DisableHealthServiceAccessForOrganization
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSHealth_20160804.DisableHealthServiceAccessForOrganization"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON
           DisableHealthServiceAccessForOrganization
         where
        toJSON = const (Object mempty)

instance ToPath
           DisableHealthServiceAccessForOrganization
         where
        toPath = const "/"

instance ToQuery
           DisableHealthServiceAccessForOrganization
         where
        toQuery = const mempty

-- | /See:/ 'disableHealthServiceAccessForOrganizationResponse' smart constructor.
data DisableHealthServiceAccessForOrganizationResponse = DisableHealthServiceAccessForOrganizationResponse'
                                                           deriving (Eq, Read,
                                                                     Show, Data,
                                                                     Typeable,
                                                                     Generic)

-- | Creates a value of 'DisableHealthServiceAccessForOrganizationResponse' with the minimum fields required to make a request.
--
disableHealthServiceAccessForOrganizationResponse
    :: DisableHealthServiceAccessForOrganizationResponse
disableHealthServiceAccessForOrganizationResponse
  = DisableHealthServiceAccessForOrganizationResponse'

instance NFData
           DisableHealthServiceAccessForOrganizationResponse
         where
