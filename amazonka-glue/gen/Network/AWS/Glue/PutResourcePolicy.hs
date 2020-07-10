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
-- Module      : Network.AWS.Glue.PutResourcePolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the Data Catalog resource policy for access control.
--
--
module Network.AWS.Glue.PutResourcePolicy
    (
    -- * Creating a Request
      putResourcePolicy
    , PutResourcePolicy
    -- * Request Lenses
    , prpPolicyExistsCondition
    , prpPolicyHashCondition
    , prpPolicyInJSON

    -- * Destructuring the Response
    , putResourcePolicyResponse
    , PutResourcePolicyResponse
    -- * Response Lenses
    , prprsPolicyHash
    , prprsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putResourcePolicy' smart constructor.
data PutResourcePolicy = PutResourcePolicy'{_prpPolicyExistsCondition
                                            :: !(Maybe ExistCondition),
                                            _prpPolicyHashCondition ::
                                            !(Maybe Text),
                                            _prpPolicyInJSON :: !Text}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutResourcePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prpPolicyExistsCondition' - A value of @MUST_EXIST@ is used to update a policy. A value of @NOT_EXIST@ is used to create a new policy. If a value of @NONE@ or a null value is used, the call will not depend on the existence of a policy.
--
-- * 'prpPolicyHashCondition' - The hash value returned when the previous policy was set using @PutResourcePolicy@ . Its purpose is to prevent concurrent modifications of a policy. Do not use this parameter if no previous policy has been set.
--
-- * 'prpPolicyInJSON' - Contains the policy document to set, in JSON format.
putResourcePolicy
    :: Text -- ^ 'prpPolicyInJSON'
    -> PutResourcePolicy
putResourcePolicy pPolicyInJSON_
  = PutResourcePolicy'{_prpPolicyExistsCondition =
                         Nothing,
                       _prpPolicyHashCondition = Nothing,
                       _prpPolicyInJSON = pPolicyInJSON_}

-- | A value of @MUST_EXIST@ is used to update a policy. A value of @NOT_EXIST@ is used to create a new policy. If a value of @NONE@ or a null value is used, the call will not depend on the existence of a policy.
prpPolicyExistsCondition :: Lens' PutResourcePolicy (Maybe ExistCondition)
prpPolicyExistsCondition = lens _prpPolicyExistsCondition (\ s a -> s{_prpPolicyExistsCondition = a})

-- | The hash value returned when the previous policy was set using @PutResourcePolicy@ . Its purpose is to prevent concurrent modifications of a policy. Do not use this parameter if no previous policy has been set.
prpPolicyHashCondition :: Lens' PutResourcePolicy (Maybe Text)
prpPolicyHashCondition = lens _prpPolicyHashCondition (\ s a -> s{_prpPolicyHashCondition = a})

-- | Contains the policy document to set, in JSON format.
prpPolicyInJSON :: Lens' PutResourcePolicy Text
prpPolicyInJSON = lens _prpPolicyInJSON (\ s a -> s{_prpPolicyInJSON = a})

instance AWSRequest PutResourcePolicy where
        type Rs PutResourcePolicy = PutResourcePolicyResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 PutResourcePolicyResponse' <$>
                   (x .?> "PolicyHash") <*> (pure (fromEnum s)))

instance Hashable PutResourcePolicy where

instance NFData PutResourcePolicy where

instance ToHeaders PutResourcePolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.PutResourcePolicy" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutResourcePolicy where
        toJSON PutResourcePolicy'{..}
          = object
              (catMaybes
                 [("PolicyExistsCondition" .=) <$>
                    _prpPolicyExistsCondition,
                  ("PolicyHashCondition" .=) <$>
                    _prpPolicyHashCondition,
                  Just ("PolicyInJson" .= _prpPolicyInJSON)])

instance ToPath PutResourcePolicy where
        toPath = const "/"

instance ToQuery PutResourcePolicy where
        toQuery = const mempty

-- | /See:/ 'putResourcePolicyResponse' smart constructor.
data PutResourcePolicyResponse = PutResourcePolicyResponse'{_prprsPolicyHash
                                                            :: !(Maybe Text),
                                                            _prprsResponseStatus
                                                            :: !Int}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'PutResourcePolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prprsPolicyHash' - A hash of the policy that has just been set. This must be included in a subsequent call that overwrites or updates this policy.
--
-- * 'prprsResponseStatus' - -- | The response status code.
putResourcePolicyResponse
    :: Int -- ^ 'prprsResponseStatus'
    -> PutResourcePolicyResponse
putResourcePolicyResponse pResponseStatus_
  = PutResourcePolicyResponse'{_prprsPolicyHash =
                                 Nothing,
                               _prprsResponseStatus = pResponseStatus_}

-- | A hash of the policy that has just been set. This must be included in a subsequent call that overwrites or updates this policy.
prprsPolicyHash :: Lens' PutResourcePolicyResponse (Maybe Text)
prprsPolicyHash = lens _prprsPolicyHash (\ s a -> s{_prprsPolicyHash = a})

-- | -- | The response status code.
prprsResponseStatus :: Lens' PutResourcePolicyResponse Int
prprsResponseStatus = lens _prprsResponseStatus (\ s a -> s{_prprsResponseStatus = a})

instance NFData PutResourcePolicyResponse where
