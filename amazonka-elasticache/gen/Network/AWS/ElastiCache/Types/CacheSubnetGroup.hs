{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.CacheSubnetGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.CacheSubnetGroup where

import Network.AWS.ElastiCache.Types.Subnet
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of one of the following operations:
--
--
--     * @CreateCacheSubnetGroup@ 
--
--     * @ModifyCacheSubnetGroup@ 
--
--
--
--
-- /See:/ 'cacheSubnetGroup' smart constructor.
data CacheSubnetGroup = CacheSubnetGroup'{_csgVPCId
                                          :: !(Maybe Text),
                                          _csgSubnets :: !(Maybe [Subnet]),
                                          _csgCacheSubnetGroupName ::
                                          !(Maybe Text),
                                          _csgCacheSubnetGroupDescription ::
                                          !(Maybe Text)}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CacheSubnetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csgVPCId' - The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet group.
--
-- * 'csgSubnets' - A list of subnets associated with the cache subnet group.
--
-- * 'csgCacheSubnetGroupName' - The name of the cache subnet group.
--
-- * 'csgCacheSubnetGroupDescription' - The description of the cache subnet group.
cacheSubnetGroup
    :: CacheSubnetGroup
cacheSubnetGroup
  = CacheSubnetGroup'{_csgVPCId = Nothing,
                      _csgSubnets = Nothing,
                      _csgCacheSubnetGroupName = Nothing,
                      _csgCacheSubnetGroupDescription = Nothing}

-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet group.
csgVPCId :: Lens' CacheSubnetGroup (Maybe Text)
csgVPCId = lens _csgVPCId (\ s a -> s{_csgVPCId = a})

-- | A list of subnets associated with the cache subnet group.
csgSubnets :: Lens' CacheSubnetGroup [Subnet]
csgSubnets = lens _csgSubnets (\ s a -> s{_csgSubnets = a}) . _Default . _Coerce

-- | The name of the cache subnet group.
csgCacheSubnetGroupName :: Lens' CacheSubnetGroup (Maybe Text)
csgCacheSubnetGroupName = lens _csgCacheSubnetGroupName (\ s a -> s{_csgCacheSubnetGroupName = a})

-- | The description of the cache subnet group.
csgCacheSubnetGroupDescription :: Lens' CacheSubnetGroup (Maybe Text)
csgCacheSubnetGroupDescription = lens _csgCacheSubnetGroupDescription (\ s a -> s{_csgCacheSubnetGroupDescription = a})

instance FromXML CacheSubnetGroup where
        parseXML x
          = CacheSubnetGroup' <$>
              (x .@? "VpcId") <*>
                (x .@? "Subnets" .!@ mempty >>=
                   may (parseXMLList "Subnet"))
                <*> (x .@? "CacheSubnetGroupName")
                <*> (x .@? "CacheSubnetGroupDescription")

instance Hashable CacheSubnetGroup where

instance NFData CacheSubnetGroup where
