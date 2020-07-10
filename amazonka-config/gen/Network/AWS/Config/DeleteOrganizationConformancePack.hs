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
-- Module      : Network.AWS.Config.DeleteOrganizationConformancePack
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified organization conformance pack and all of the config rules and remediation actions from all member accounts in that organization. Only a master account can delete an organization conformance pack.
--
--
-- AWS Config sets the state of a conformance pack to DELETE_IN_PROGRESS until the deletion is complete. You cannot update a conformance pack while it is in this state. 
--
module Network.AWS.Config.DeleteOrganizationConformancePack
    (
    -- * Creating a Request
      deleteOrganizationConformancePack
    , DeleteOrganizationConformancePack
    -- * Request Lenses
    , docpOrganizationConformancePackName

    -- * Destructuring the Response
    , deleteOrganizationConformancePackResponse
    , DeleteOrganizationConformancePackResponse
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteOrganizationConformancePack' smart constructor.
newtype DeleteOrganizationConformancePack = DeleteOrganizationConformancePack'{_docpOrganizationConformancePackName
                                                                               ::
                                                                               Text}
                                              deriving (Eq, Read, Show, Data,
                                                        Typeable, Generic)

-- | Creates a value of 'DeleteOrganizationConformancePack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'docpOrganizationConformancePackName' - The name of organization conformance pack that you want to delete.
deleteOrganizationConformancePack
    :: Text -- ^ 'docpOrganizationConformancePackName'
    -> DeleteOrganizationConformancePack
deleteOrganizationConformancePack
  pOrganizationConformancePackName_
  = DeleteOrganizationConformancePack'{_docpOrganizationConformancePackName
                                         = pOrganizationConformancePackName_}

-- | The name of organization conformance pack that you want to delete.
docpOrganizationConformancePackName :: Lens' DeleteOrganizationConformancePack Text
docpOrganizationConformancePackName = lens _docpOrganizationConformancePackName (\ s a -> s{_docpOrganizationConformancePackName = a})

instance AWSRequest DeleteOrganizationConformancePack
         where
        type Rs DeleteOrganizationConformancePack =
             DeleteOrganizationConformancePackResponse
        request = postJSON config
        response
          = receiveNull
              DeleteOrganizationConformancePackResponse'

instance Hashable DeleteOrganizationConformancePack
         where

instance NFData DeleteOrganizationConformancePack
         where

instance ToHeaders DeleteOrganizationConformancePack
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.DeleteOrganizationConformancePack"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteOrganizationConformancePack
         where
        toJSON DeleteOrganizationConformancePack'{..}
          = object
              (catMaybes
                 [Just
                    ("OrganizationConformancePackName" .=
                       _docpOrganizationConformancePackName)])

instance ToPath DeleteOrganizationConformancePack
         where
        toPath = const "/"

instance ToQuery DeleteOrganizationConformancePack
         where
        toQuery = const mempty

-- | /See:/ 'deleteOrganizationConformancePackResponse' smart constructor.
data DeleteOrganizationConformancePackResponse = DeleteOrganizationConformancePackResponse'
                                                   deriving (Eq, Read, Show,
                                                             Data, Typeable,
                                                             Generic)

-- | Creates a value of 'DeleteOrganizationConformancePackResponse' with the minimum fields required to make a request.
--
deleteOrganizationConformancePackResponse
    :: DeleteOrganizationConformancePackResponse
deleteOrganizationConformancePackResponse
  = DeleteOrganizationConformancePackResponse'

instance NFData
           DeleteOrganizationConformancePackResponse
         where
