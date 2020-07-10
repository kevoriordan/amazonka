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
-- Module      : Network.AWS.ElastiCache.CreateGlobalReplicationGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Global Datastore for Redis offers fully managed, fast, reliable and secure cross-region replication. Using Global Datastore for Redis, you can create cross-region read replica clusters for ElastiCache for Redis to enable low-latency reads and disaster recovery across regions. For more information, see </AmazonElastiCache/latest/red-ug/Redis-Global-Clusters.html Replication Across Regions Using Global Datastore> . 
--
--
--     * The __GlobalReplicationGroupId__ is the name of the Global Datastore.
--
--     * The __PrimaryReplicationGroupId__ represents the name of the primary cluster that accepts writes and will replicate updates to the secondary cluster.
--
--
--
module Network.AWS.ElastiCache.CreateGlobalReplicationGroup
    (
    -- * Creating a Request
      createGlobalReplicationGroup
    , CreateGlobalReplicationGroup
    -- * Request Lenses
    , cgrgGlobalReplicationGroupDescription
    , cgrgGlobalReplicationGroupIdSuffix
    , cgrgPrimaryReplicationGroupId

    -- * Destructuring the Response
    , createGlobalReplicationGroupResponse
    , CreateGlobalReplicationGroupResponse
    -- * Response Lenses
    , cgrgrsGlobalReplicationGroup
    , cgrgrsResponseStatus
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.ElastiCache.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createGlobalReplicationGroup' smart constructor.
data CreateGlobalReplicationGroup = CreateGlobalReplicationGroup'{_cgrgGlobalReplicationGroupDescription
                                                                  ::
                                                                  !(Maybe Text),
                                                                  _cgrgGlobalReplicationGroupIdSuffix
                                                                  :: !Text,
                                                                  _cgrgPrimaryReplicationGroupId
                                                                  :: !Text}
                                      deriving (Eq, Read, Show, Data, Typeable,
                                                Generic)

-- | Creates a value of 'CreateGlobalReplicationGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgrgGlobalReplicationGroupDescription' - Provides details of the Global Datastore
--
-- * 'cgrgGlobalReplicationGroupIdSuffix' - The suffix for name of a Global Datastore. The suffix guarantees uniqueness of the Global Datastore name across multiple regions.
--
-- * 'cgrgPrimaryReplicationGroupId' - The name of the primary cluster that accepts writes and will replicate updates to the secondary cluster.
createGlobalReplicationGroup
    :: Text -- ^ 'cgrgGlobalReplicationGroupIdSuffix'
    -> Text -- ^ 'cgrgPrimaryReplicationGroupId'
    -> CreateGlobalReplicationGroup
createGlobalReplicationGroup
  pGlobalReplicationGroupIdSuffix_
  pPrimaryReplicationGroupId_
  = CreateGlobalReplicationGroup'{_cgrgGlobalReplicationGroupDescription
                                    = Nothing,
                                  _cgrgGlobalReplicationGroupIdSuffix =
                                    pGlobalReplicationGroupIdSuffix_,
                                  _cgrgPrimaryReplicationGroupId =
                                    pPrimaryReplicationGroupId_}

-- | Provides details of the Global Datastore
cgrgGlobalReplicationGroupDescription :: Lens' CreateGlobalReplicationGroup (Maybe Text)
cgrgGlobalReplicationGroupDescription = lens _cgrgGlobalReplicationGroupDescription (\ s a -> s{_cgrgGlobalReplicationGroupDescription = a})

-- | The suffix for name of a Global Datastore. The suffix guarantees uniqueness of the Global Datastore name across multiple regions.
cgrgGlobalReplicationGroupIdSuffix :: Lens' CreateGlobalReplicationGroup Text
cgrgGlobalReplicationGroupIdSuffix = lens _cgrgGlobalReplicationGroupIdSuffix (\ s a -> s{_cgrgGlobalReplicationGroupIdSuffix = a})

-- | The name of the primary cluster that accepts writes and will replicate updates to the secondary cluster.
cgrgPrimaryReplicationGroupId :: Lens' CreateGlobalReplicationGroup Text
cgrgPrimaryReplicationGroupId = lens _cgrgPrimaryReplicationGroupId (\ s a -> s{_cgrgPrimaryReplicationGroupId = a})

instance AWSRequest CreateGlobalReplicationGroup
         where
        type Rs CreateGlobalReplicationGroup =
             CreateGlobalReplicationGroupResponse
        request = postQuery elastiCache
        response
          = receiveXMLWrapper
              "CreateGlobalReplicationGroupResult"
              (\ s h x ->
                 CreateGlobalReplicationGroupResponse' <$>
                   (x .@? "GlobalReplicationGroup") <*>
                     (pure (fromEnum s)))

instance Hashable CreateGlobalReplicationGroup where

instance NFData CreateGlobalReplicationGroup where

instance ToHeaders CreateGlobalReplicationGroup where
        toHeaders = const mempty

instance ToPath CreateGlobalReplicationGroup where
        toPath = const "/"

instance ToQuery CreateGlobalReplicationGroup where
        toQuery CreateGlobalReplicationGroup'{..}
          = mconcat
              ["Action" =:
                 ("CreateGlobalReplicationGroup" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "GlobalReplicationGroupDescription" =:
                 _cgrgGlobalReplicationGroupDescription,
               "GlobalReplicationGroupIdSuffix" =:
                 _cgrgGlobalReplicationGroupIdSuffix,
               "PrimaryReplicationGroupId" =:
                 _cgrgPrimaryReplicationGroupId]

-- | /See:/ 'createGlobalReplicationGroupResponse' smart constructor.
data CreateGlobalReplicationGroupResponse = CreateGlobalReplicationGroupResponse'{_cgrgrsGlobalReplicationGroup
                                                                                  ::
                                                                                  !(Maybe
                                                                                      GlobalReplicationGroup),
                                                                                  _cgrgrsResponseStatus
                                                                                  ::
                                                                                  !Int}
                                              deriving (Eq, Read, Show, Data,
                                                        Typeable, Generic)

-- | Creates a value of 'CreateGlobalReplicationGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgrgrsGlobalReplicationGroup' - Undocumented member.
--
-- * 'cgrgrsResponseStatus' - -- | The response status code.
createGlobalReplicationGroupResponse
    :: Int -- ^ 'cgrgrsResponseStatus'
    -> CreateGlobalReplicationGroupResponse
createGlobalReplicationGroupResponse pResponseStatus_
  = CreateGlobalReplicationGroupResponse'{_cgrgrsGlobalReplicationGroup
                                            = Nothing,
                                          _cgrgrsResponseStatus =
                                            pResponseStatus_}

-- | Undocumented member.
cgrgrsGlobalReplicationGroup :: Lens' CreateGlobalReplicationGroupResponse (Maybe GlobalReplicationGroup)
cgrgrsGlobalReplicationGroup = lens _cgrgrsGlobalReplicationGroup (\ s a -> s{_cgrgrsGlobalReplicationGroup = a})

-- | -- | The response status code.
cgrgrsResponseStatus :: Lens' CreateGlobalReplicationGroupResponse Int
cgrgrsResponseStatus = lens _cgrgrsResponseStatus (\ s a -> s{_cgrgrsResponseStatus = a})

instance NFData CreateGlobalReplicationGroupResponse
         where
