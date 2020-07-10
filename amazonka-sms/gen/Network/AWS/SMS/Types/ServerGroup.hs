{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ServerGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types.ServerGroup where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SMS.Types.Server

-- | A logical grouping of servers.
--
--
--
-- /See:/ 'serverGroup' smart constructor.
data ServerGroup = ServerGroup'{_sgServerList ::
                                !(Maybe [Server]),
                                _sgName :: !(Maybe Text),
                                _sgServerGroupId :: !(Maybe Text)}
                     deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServerGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgServerList' - List of servers belonging to a server group.
--
-- * 'sgName' - Name of a server group.
--
-- * 'sgServerGroupId' - Identifier of a server group.
serverGroup
    :: ServerGroup
serverGroup
  = ServerGroup'{_sgServerList = Nothing,
                 _sgName = Nothing, _sgServerGroupId = Nothing}

-- | List of servers belonging to a server group.
sgServerList :: Lens' ServerGroup [Server]
sgServerList = lens _sgServerList (\ s a -> s{_sgServerList = a}) . _Default . _Coerce

-- | Name of a server group.
sgName :: Lens' ServerGroup (Maybe Text)
sgName = lens _sgName (\ s a -> s{_sgName = a})

-- | Identifier of a server group.
sgServerGroupId :: Lens' ServerGroup (Maybe Text)
sgServerGroupId = lens _sgServerGroupId (\ s a -> s{_sgServerGroupId = a})

instance FromJSON ServerGroup where
        parseJSON
          = withObject "ServerGroup"
              (\ x ->
                 ServerGroup' <$>
                   (x .:? "serverList" .!= mempty) <*> (x .:? "name")
                     <*> (x .:? "serverGroupId"))

instance Hashable ServerGroup where

instance NFData ServerGroup where

instance ToJSON ServerGroup where
        toJSON ServerGroup'{..}
          = object
              (catMaybes
                 [("serverList" .=) <$> _sgServerList,
                  ("name" .=) <$> _sgName,
                  ("serverGroupId" .=) <$> _sgServerGroupId])
