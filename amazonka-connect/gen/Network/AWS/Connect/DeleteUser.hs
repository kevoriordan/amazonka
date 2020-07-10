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
-- Module      : Network.AWS.Connect.DeleteUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user account from the specified Amazon Connect instance.
--
--
module Network.AWS.Connect.DeleteUser
    (
    -- * Creating a Request
      deleteUser
    , DeleteUser
    -- * Request Lenses
    , dInstanceId
    , dUserId

    -- * Destructuring the Response
    , deleteUserResponse
    , DeleteUserResponse
    ) where

import Network.AWS.Connect.Types
import Network.AWS.Connect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteUser' smart constructor.
data DeleteUser = DeleteUser'{_dInstanceId :: !Text,
                              _dUserId :: !Text}
                    deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dInstanceId' - The identifier of the Amazon Connect instance.
--
-- * 'dUserId' - The identifier of the user.
deleteUser
    :: Text -- ^ 'dInstanceId'
    -> Text -- ^ 'dUserId'
    -> DeleteUser
deleteUser pInstanceId_ pUserId_
  = DeleteUser'{_dInstanceId = pInstanceId_,
                _dUserId = pUserId_}

-- | The identifier of the Amazon Connect instance.
dInstanceId :: Lens' DeleteUser Text
dInstanceId = lens _dInstanceId (\ s a -> s{_dInstanceId = a})

-- | The identifier of the user.
dUserId :: Lens' DeleteUser Text
dUserId = lens _dUserId (\ s a -> s{_dUserId = a})

instance AWSRequest DeleteUser where
        type Rs DeleteUser = DeleteUserResponse
        request = delete connect
        response = receiveNull DeleteUserResponse'

instance Hashable DeleteUser where

instance NFData DeleteUser where

instance ToHeaders DeleteUser where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteUser where
        toPath DeleteUser'{..}
          = mconcat
              ["/users/", toBS _dInstanceId, "/", toBS _dUserId]

instance ToQuery DeleteUser where
        toQuery = const mempty

-- | /See:/ 'deleteUserResponse' smart constructor.
data DeleteUserResponse = DeleteUserResponse'
                            deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteUserResponse' with the minimum fields required to make a request.
--
deleteUserResponse
    :: DeleteUserResponse
deleteUserResponse = DeleteUserResponse'

instance NFData DeleteUserResponse where
