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
-- Module      : Network.AWS.StepFunctions.UntagResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove a tag from a Step Functions resource
--
--
module Network.AWS.StepFunctions.UntagResource
    (
    -- * Creating a Request
      untagResource
    , UntagResource
    -- * Request Lenses
    , urResourceARN
    , urTagKeys

    -- * Destructuring the Response
    , untagResourceResponse
    , UntagResourceResponse
    -- * Response Lenses
    , urrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StepFunctions.Types
import Network.AWS.StepFunctions.Types.Product

-- | /See:/ 'untagResource' smart constructor.
data UntagResource = UntagResource'{_urResourceARN ::
                                    !Text,
                                    _urTagKeys :: ![Text]}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UntagResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urResourceARN' - The Amazon Resource Name (ARN) for the Step Functions state machine or activity.
--
-- * 'urTagKeys' - The list of tags to remove from the resource.
untagResource
    :: Text -- ^ 'urResourceARN'
    -> UntagResource
untagResource pResourceARN_
  = UntagResource'{_urResourceARN = pResourceARN_,
                   _urTagKeys = mempty}

-- | The Amazon Resource Name (ARN) for the Step Functions state machine or activity.
urResourceARN :: Lens' UntagResource Text
urResourceARN = lens _urResourceARN (\ s a -> s{_urResourceARN = a})

-- | The list of tags to remove from the resource.
urTagKeys :: Lens' UntagResource [Text]
urTagKeys = lens _urTagKeys (\ s a -> s{_urTagKeys = a}) . _Coerce

instance AWSRequest UntagResource where
        type Rs UntagResource = UntagResourceResponse
        request = postJSON stepFunctions
        response
          = receiveEmpty
              (\ s h x ->
                 UntagResourceResponse' <$> (pure (fromEnum s)))

instance Hashable UntagResource where

instance NFData UntagResource where

instance ToHeaders UntagResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSStepFunctions.UntagResource" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON UntagResource where
        toJSON UntagResource'{..}
          = object
              (catMaybes
                 [Just ("resourceArn" .= _urResourceARN),
                  Just ("tagKeys" .= _urTagKeys)])

instance ToPath UntagResource where
        toPath = const "/"

instance ToQuery UntagResource where
        toQuery = const mempty

-- | /See:/ 'untagResourceResponse' smart constructor.
newtype UntagResourceResponse = UntagResourceResponse'{_urrsResponseStatus
                                                       :: Int}
                                  deriving (Eq, Read, Show, Data, Typeable,
                                            Generic)

-- | Creates a value of 'UntagResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urrsResponseStatus' - -- | The response status code.
untagResourceResponse
    :: Int -- ^ 'urrsResponseStatus'
    -> UntagResourceResponse
untagResourceResponse pResponseStatus_
  = UntagResourceResponse'{_urrsResponseStatus =
                             pResponseStatus_}

-- | -- | The response status code.
urrsResponseStatus :: Lens' UntagResourceResponse Int
urrsResponseStatus = lens _urrsResponseStatus (\ s a -> s{_urrsResponseStatus = a})

instance NFData UntagResourceResponse where
