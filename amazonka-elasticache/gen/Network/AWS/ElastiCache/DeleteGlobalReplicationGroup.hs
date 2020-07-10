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
-- Module      : Network.AWS.ElastiCache.DeleteGlobalReplicationGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deleting a Global Datastore is a two-step process: 
--
--
--     * First, you must 'DisassociateGlobalReplicationGroup' to remove the secondary clusters in the Global Datastore.
--
--     * Once the Global Datastore contains only the primary cluster, you can use DeleteGlobalReplicationGroup API to delete the Global Datastore while retainining the primary cluster using Retain…= true.
--
--
--
-- Since the Global Datastore has only a primary cluster, you can delete the Global Datastore while retaining the primary by setting @RetainPrimaryCluster=true@ .
--
-- When you receive a successful response from this operation, Amazon ElastiCache immediately begins deleting the selected resources; you cannot cancel or revert this operation.
--
module Network.AWS.ElastiCache.DeleteGlobalReplicationGroup
    (
    -- * Creating a Request
      deleteGlobalReplicationGroup
    , DeleteGlobalReplicationGroup
    -- * Request Lenses
    , dGlobalReplicationGroupId
    , dRetainPrimaryReplicationGroup

    -- * Destructuring the Response
    , deleteGlobalReplicationGroupResponse
    , DeleteGlobalReplicationGroupResponse
    -- * Response Lenses
    , dltglblrplctngrprsGlobalReplicationGroup
    , dltglblrplctngrprsResponseStatus
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.ElastiCache.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteGlobalReplicationGroup' smart constructor.
data DeleteGlobalReplicationGroup = DeleteGlobalReplicationGroup'{_dGlobalReplicationGroupId
                                                                  :: !Text,
                                                                  _dRetainPrimaryReplicationGroup
                                                                  :: !Bool}
                                      deriving (Eq, Read, Show, Data, Typeable,
                                                Generic)

-- | Creates a value of 'DeleteGlobalReplicationGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dGlobalReplicationGroupId' - The name of the Global Datastore
--
-- * 'dRetainPrimaryReplicationGroup' - If set to @true@ , the primary replication is retained as a standalone replication group. 
deleteGlobalReplicationGroup
    :: Text -- ^ 'dGlobalReplicationGroupId'
    -> Bool -- ^ 'dRetainPrimaryReplicationGroup'
    -> DeleteGlobalReplicationGroup
deleteGlobalReplicationGroup
  pGlobalReplicationGroupId_
  pRetainPrimaryReplicationGroup_
  = DeleteGlobalReplicationGroup'{_dGlobalReplicationGroupId
                                    = pGlobalReplicationGroupId_,
                                  _dRetainPrimaryReplicationGroup =
                                    pRetainPrimaryReplicationGroup_}

-- | The name of the Global Datastore
dGlobalReplicationGroupId :: Lens' DeleteGlobalReplicationGroup Text
dGlobalReplicationGroupId = lens _dGlobalReplicationGroupId (\ s a -> s{_dGlobalReplicationGroupId = a})

-- | If set to @true@ , the primary replication is retained as a standalone replication group. 
dRetainPrimaryReplicationGroup :: Lens' DeleteGlobalReplicationGroup Bool
dRetainPrimaryReplicationGroup = lens _dRetainPrimaryReplicationGroup (\ s a -> s{_dRetainPrimaryReplicationGroup = a})

instance AWSRequest DeleteGlobalReplicationGroup
         where
        type Rs DeleteGlobalReplicationGroup =
             DeleteGlobalReplicationGroupResponse
        request = postQuery elastiCache
        response
          = receiveXMLWrapper
              "DeleteGlobalReplicationGroupResult"
              (\ s h x ->
                 DeleteGlobalReplicationGroupResponse' <$>
                   (x .@? "GlobalReplicationGroup") <*>
                     (pure (fromEnum s)))

instance Hashable DeleteGlobalReplicationGroup where

instance NFData DeleteGlobalReplicationGroup where

instance ToHeaders DeleteGlobalReplicationGroup where
        toHeaders = const mempty

instance ToPath DeleteGlobalReplicationGroup where
        toPath = const "/"

instance ToQuery DeleteGlobalReplicationGroup where
        toQuery DeleteGlobalReplicationGroup'{..}
          = mconcat
              ["Action" =:
                 ("DeleteGlobalReplicationGroup" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "GlobalReplicationGroupId" =:
                 _dGlobalReplicationGroupId,
               "RetainPrimaryReplicationGroup" =:
                 _dRetainPrimaryReplicationGroup]

-- | /See:/ 'deleteGlobalReplicationGroupResponse' smart constructor.
data DeleteGlobalReplicationGroupResponse = DeleteGlobalReplicationGroupResponse'{_dltglblrplctngrprsGlobalReplicationGroup
                                                                                  ::
                                                                                  !(Maybe
                                                                                      GlobalReplicationGroup),
                                                                                  _dltglblrplctngrprsResponseStatus
                                                                                  ::
                                                                                  !Int}
                                              deriving (Eq, Read, Show, Data,
                                                        Typeable, Generic)

-- | Creates a value of 'DeleteGlobalReplicationGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dltglblrplctngrprsGlobalReplicationGroup' - Undocumented member.
--
-- * 'dltglblrplctngrprsResponseStatus' - -- | The response status code.
deleteGlobalReplicationGroupResponse
    :: Int -- ^ 'dltglblrplctngrprsResponseStatus'
    -> DeleteGlobalReplicationGroupResponse
deleteGlobalReplicationGroupResponse pResponseStatus_
  = DeleteGlobalReplicationGroupResponse'{_dltglblrplctngrprsGlobalReplicationGroup
                                            = Nothing,
                                          _dltglblrplctngrprsResponseStatus =
                                            pResponseStatus_}

-- | Undocumented member.
dltglblrplctngrprsGlobalReplicationGroup :: Lens' DeleteGlobalReplicationGroupResponse (Maybe GlobalReplicationGroup)
dltglblrplctngrprsGlobalReplicationGroup = lens _dltglblrplctngrprsGlobalReplicationGroup (\ s a -> s{_dltglblrplctngrprsGlobalReplicationGroup = a})

-- | -- | The response status code.
dltglblrplctngrprsResponseStatus :: Lens' DeleteGlobalReplicationGroupResponse Int
dltglblrplctngrprsResponseStatus = lens _dltglblrplctngrprsResponseStatus (\ s a -> s{_dltglblrplctngrprsResponseStatus = a})

instance NFData DeleteGlobalReplicationGroupResponse
         where
