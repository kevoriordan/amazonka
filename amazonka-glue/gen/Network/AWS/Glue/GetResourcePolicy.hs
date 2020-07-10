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
-- Module      : Network.AWS.Glue.GetResourcePolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a specified resource policy.
--
--
module Network.AWS.Glue.GetResourcePolicy
    (
    -- * Creating a Request
      getResourcePolicy
    , GetResourcePolicy

    -- * Destructuring the Response
    , getResourcePolicyResponse
    , GetResourcePolicyResponse
    -- * Response Lenses
    , grprsPolicyInJSON
    , grprsUpdateTime
    , grprsPolicyHash
    , grprsCreateTime
    , grprsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getResourcePolicy' smart constructor.
data GetResourcePolicy = GetResourcePolicy'
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetResourcePolicy' with the minimum fields required to make a request.
--
getResourcePolicy
    :: GetResourcePolicy
getResourcePolicy = GetResourcePolicy'

instance AWSRequest GetResourcePolicy where
        type Rs GetResourcePolicy = GetResourcePolicyResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 GetResourcePolicyResponse' <$>
                   (x .?> "PolicyInJson") <*> (x .?> "UpdateTime") <*>
                     (x .?> "PolicyHash")
                     <*> (x .?> "CreateTime")
                     <*> (pure (fromEnum s)))

instance Hashable GetResourcePolicy where

instance NFData GetResourcePolicy where

instance ToHeaders GetResourcePolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.GetResourcePolicy" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetResourcePolicy where
        toJSON = const (Object mempty)

instance ToPath GetResourcePolicy where
        toPath = const "/"

instance ToQuery GetResourcePolicy where
        toQuery = const mempty

-- | /See:/ 'getResourcePolicyResponse' smart constructor.
data GetResourcePolicyResponse = GetResourcePolicyResponse'{_grprsPolicyInJSON
                                                            :: !(Maybe Text),
                                                            _grprsUpdateTime ::
                                                            !(Maybe POSIX),
                                                            _grprsPolicyHash ::
                                                            !(Maybe Text),
                                                            _grprsCreateTime ::
                                                            !(Maybe POSIX),
                                                            _grprsResponseStatus
                                                            :: !Int}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'GetResourcePolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grprsPolicyInJSON' - Contains the requested policy document, in JSON format.
--
-- * 'grprsUpdateTime' - The date and time at which the policy was last updated.
--
-- * 'grprsPolicyHash' - Contains the hash value associated with this policy.
--
-- * 'grprsCreateTime' - The date and time at which the policy was created.
--
-- * 'grprsResponseStatus' - -- | The response status code.
getResourcePolicyResponse
    :: Int -- ^ 'grprsResponseStatus'
    -> GetResourcePolicyResponse
getResourcePolicyResponse pResponseStatus_
  = GetResourcePolicyResponse'{_grprsPolicyInJSON =
                                 Nothing,
                               _grprsUpdateTime = Nothing,
                               _grprsPolicyHash = Nothing,
                               _grprsCreateTime = Nothing,
                               _grprsResponseStatus = pResponseStatus_}

-- | Contains the requested policy document, in JSON format.
grprsPolicyInJSON :: Lens' GetResourcePolicyResponse (Maybe Text)
grprsPolicyInJSON = lens _grprsPolicyInJSON (\ s a -> s{_grprsPolicyInJSON = a})

-- | The date and time at which the policy was last updated.
grprsUpdateTime :: Lens' GetResourcePolicyResponse (Maybe UTCTime)
grprsUpdateTime = lens _grprsUpdateTime (\ s a -> s{_grprsUpdateTime = a}) . mapping _Time

-- | Contains the hash value associated with this policy.
grprsPolicyHash :: Lens' GetResourcePolicyResponse (Maybe Text)
grprsPolicyHash = lens _grprsPolicyHash (\ s a -> s{_grprsPolicyHash = a})

-- | The date and time at which the policy was created.
grprsCreateTime :: Lens' GetResourcePolicyResponse (Maybe UTCTime)
grprsCreateTime = lens _grprsCreateTime (\ s a -> s{_grprsCreateTime = a}) . mapping _Time

-- | -- | The response status code.
grprsResponseStatus :: Lens' GetResourcePolicyResponse Int
grprsResponseStatus = lens _grprsResponseStatus (\ s a -> s{_grprsResponseStatus = a})

instance NFData GetResourcePolicyResponse where
