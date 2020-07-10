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
-- Module      : Network.AWS.Organizations.UntagResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a tag from the specified resource. 
--
--
-- Currently, you can tag and untag accounts in AWS Organizations.
--
-- This operation can be called only from the organization's master account.
--
module Network.AWS.Organizations.UntagResource
    (
    -- * Creating a Request
      untagResource
    , UntagResource
    -- * Request Lenses
    , urResourceId
    , urTagKeys

    -- * Destructuring the Response
    , untagResourceResponse
    , UntagResourceResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'untagResource' smart constructor.
data UntagResource = UntagResource'{_urResourceId ::
                                    !Text,
                                    _urTagKeys :: ![Text]}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UntagResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urResourceId' - The ID of the resource to remove the tag from.
--
-- * 'urTagKeys' - The tag to remove from the specified resource.
untagResource
    :: Text -- ^ 'urResourceId'
    -> UntagResource
untagResource pResourceId_
  = UntagResource'{_urResourceId = pResourceId_,
                   _urTagKeys = mempty}

-- | The ID of the resource to remove the tag from.
urResourceId :: Lens' UntagResource Text
urResourceId = lens _urResourceId (\ s a -> s{_urResourceId = a})

-- | The tag to remove from the specified resource.
urTagKeys :: Lens' UntagResource [Text]
urTagKeys = lens _urTagKeys (\ s a -> s{_urTagKeys = a}) . _Coerce

instance AWSRequest UntagResource where
        type Rs UntagResource = UntagResourceResponse
        request = postJSON organizations
        response = receiveNull UntagResourceResponse'

instance Hashable UntagResource where

instance NFData UntagResource where

instance ToHeaders UntagResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.UntagResource" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UntagResource where
        toJSON UntagResource'{..}
          = object
              (catMaybes
                 [Just ("ResourceId" .= _urResourceId),
                  Just ("TagKeys" .= _urTagKeys)])

instance ToPath UntagResource where
        toPath = const "/"

instance ToQuery UntagResource where
        toQuery = const mempty

-- | /See:/ 'untagResourceResponse' smart constructor.
data UntagResourceResponse = UntagResourceResponse'
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'UntagResourceResponse' with the minimum fields required to make a request.
--
untagResourceResponse
    :: UntagResourceResponse
untagResourceResponse = UntagResourceResponse'

instance NFData UntagResourceResponse where
