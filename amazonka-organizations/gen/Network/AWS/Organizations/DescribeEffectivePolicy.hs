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
-- Module      : Network.AWS.Organizations.DescribeEffectivePolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the contents of the effective tag policy for the account. The effective tag policy is the aggregation of any tag policies the account inherits, plus any policy directly that is attached to the account. 
--
--
-- This action returns information on tag policies only.
--
-- For more information on policy inheritance, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies-inheritance.html How Policy Inheritance Works> in the /AWS Organizations User Guide/ .
--
-- This operation can be called only from the organization's master account or by a member account that is a delegated administrator for an AWS service.
--
module Network.AWS.Organizations.DescribeEffectivePolicy
    (
    -- * Creating a Request
      describeEffectivePolicy
    , DescribeEffectivePolicy
    -- * Request Lenses
    , depTargetId
    , depPolicyType

    -- * Destructuring the Response
    , describeEffectivePolicyResponse
    , DescribeEffectivePolicyResponse
    -- * Response Lenses
    , deprsEffectivePolicy
    , deprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeEffectivePolicy' smart constructor.
data DescribeEffectivePolicy = DescribeEffectivePolicy'{_depTargetId
                                                        :: !(Maybe Text),
                                                        _depPolicyType ::
                                                        !EffectivePolicyType}
                                 deriving (Eq, Read, Show, Data, Typeable,
                                           Generic)

-- | Creates a value of 'DescribeEffectivePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'depTargetId' - When you're signed in as the master account, specify the ID of the account that you want details about. Specifying an organization root or OU as the target is not supported. 
--
-- * 'depPolicyType' - The type of policy that you want information about.
describeEffectivePolicy
    :: EffectivePolicyType -- ^ 'depPolicyType'
    -> DescribeEffectivePolicy
describeEffectivePolicy pPolicyType_
  = DescribeEffectivePolicy'{_depTargetId = Nothing,
                             _depPolicyType = pPolicyType_}

-- | When you're signed in as the master account, specify the ID of the account that you want details about. Specifying an organization root or OU as the target is not supported. 
depTargetId :: Lens' DescribeEffectivePolicy (Maybe Text)
depTargetId = lens _depTargetId (\ s a -> s{_depTargetId = a})

-- | The type of policy that you want information about.
depPolicyType :: Lens' DescribeEffectivePolicy EffectivePolicyType
depPolicyType = lens _depPolicyType (\ s a -> s{_depPolicyType = a})

instance AWSRequest DescribeEffectivePolicy where
        type Rs DescribeEffectivePolicy =
             DescribeEffectivePolicyResponse
        request = postJSON organizations
        response
          = receiveJSON
              (\ s h x ->
                 DescribeEffectivePolicyResponse' <$>
                   (x .?> "EffectivePolicy") <*> (pure (fromEnum s)))

instance Hashable DescribeEffectivePolicy where

instance NFData DescribeEffectivePolicy where

instance ToHeaders DescribeEffectivePolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.DescribeEffectivePolicy"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeEffectivePolicy where
        toJSON DescribeEffectivePolicy'{..}
          = object
              (catMaybes
                 [("TargetId" .=) <$> _depTargetId,
                  Just ("PolicyType" .= _depPolicyType)])

instance ToPath DescribeEffectivePolicy where
        toPath = const "/"

instance ToQuery DescribeEffectivePolicy where
        toQuery = const mempty

-- | /See:/ 'describeEffectivePolicyResponse' smart constructor.
data DescribeEffectivePolicyResponse = DescribeEffectivePolicyResponse'{_deprsEffectivePolicy
                                                                        ::
                                                                        !(Maybe
                                                                            EffectivePolicy),
                                                                        _deprsResponseStatus
                                                                        :: !Int}
                                         deriving (Eq, Read, Show, Data,
                                                   Typeable, Generic)

-- | Creates a value of 'DescribeEffectivePolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deprsEffectivePolicy' - The contents of the effective policy.
--
-- * 'deprsResponseStatus' - -- | The response status code.
describeEffectivePolicyResponse
    :: Int -- ^ 'deprsResponseStatus'
    -> DescribeEffectivePolicyResponse
describeEffectivePolicyResponse pResponseStatus_
  = DescribeEffectivePolicyResponse'{_deprsEffectivePolicy
                                       = Nothing,
                                     _deprsResponseStatus = pResponseStatus_}

-- | The contents of the effective policy.
deprsEffectivePolicy :: Lens' DescribeEffectivePolicyResponse (Maybe EffectivePolicy)
deprsEffectivePolicy = lens _deprsEffectivePolicy (\ s a -> s{_deprsEffectivePolicy = a})

-- | -- | The response status code.
deprsResponseStatus :: Lens' DescribeEffectivePolicyResponse Int
deprsResponseStatus = lens _deprsResponseStatus (\ s a -> s{_deprsResponseStatus = a})

instance NFData DescribeEffectivePolicyResponse where
