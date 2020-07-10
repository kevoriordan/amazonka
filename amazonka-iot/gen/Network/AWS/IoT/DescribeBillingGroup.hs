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
-- Module      : Network.AWS.IoT.DescribeBillingGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a billing group.
--
--
module Network.AWS.IoT.DescribeBillingGroup
    (
    -- * Creating a Request
      describeBillingGroup
    , DescribeBillingGroup
    -- * Request Lenses
    , dBillingGroupName

    -- * Destructuring the Response
    , describeBillingGroupResponse
    , DescribeBillingGroupResponse
    -- * Response Lenses
    , dscrbbllnggrprsBillingGroupARN
    , dscrbbllnggrprsVersion
    , dscrbbllnggrprsBillingGroupProperties
    , dscrbbllnggrprsBillingGroupName
    , dscrbbllnggrprsBillingGroupId
    , dscrbbllnggrprsBillingGroupMetadata
    , dscrbbllnggrprsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeBillingGroup' smart constructor.
newtype DescribeBillingGroup = DescribeBillingGroup'{_dBillingGroupName
                                                     :: Text}
                                 deriving (Eq, Read, Show, Data, Typeable,
                                           Generic)

-- | Creates a value of 'DescribeBillingGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dBillingGroupName' - The name of the billing group.
describeBillingGroup
    :: Text -- ^ 'dBillingGroupName'
    -> DescribeBillingGroup
describeBillingGroup pBillingGroupName_
  = DescribeBillingGroup'{_dBillingGroupName =
                            pBillingGroupName_}

-- | The name of the billing group.
dBillingGroupName :: Lens' DescribeBillingGroup Text
dBillingGroupName = lens _dBillingGroupName (\ s a -> s{_dBillingGroupName = a})

instance AWSRequest DescribeBillingGroup where
        type Rs DescribeBillingGroup =
             DescribeBillingGroupResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 DescribeBillingGroupResponse' <$>
                   (x .?> "billingGroupArn") <*> (x .?> "version") <*>
                     (x .?> "billingGroupProperties")
                     <*> (x .?> "billingGroupName")
                     <*> (x .?> "billingGroupId")
                     <*> (x .?> "billingGroupMetadata")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeBillingGroup where

instance NFData DescribeBillingGroup where

instance ToHeaders DescribeBillingGroup where
        toHeaders = const mempty

instance ToPath DescribeBillingGroup where
        toPath DescribeBillingGroup'{..}
          = mconcat
              ["/billing-groups/", toBS _dBillingGroupName]

instance ToQuery DescribeBillingGroup where
        toQuery = const mempty

-- | /See:/ 'describeBillingGroupResponse' smart constructor.
data DescribeBillingGroupResponse = DescribeBillingGroupResponse'{_dscrbbllnggrprsBillingGroupARN
                                                                  ::
                                                                  !(Maybe Text),
                                                                  _dscrbbllnggrprsVersion
                                                                  ::
                                                                  !(Maybe
                                                                      Integer),
                                                                  _dscrbbllnggrprsBillingGroupProperties
                                                                  ::
                                                                  !(Maybe
                                                                      BillingGroupProperties),
                                                                  _dscrbbllnggrprsBillingGroupName
                                                                  ::
                                                                  !(Maybe Text),
                                                                  _dscrbbllnggrprsBillingGroupId
                                                                  ::
                                                                  !(Maybe Text),
                                                                  _dscrbbllnggrprsBillingGroupMetadata
                                                                  ::
                                                                  !(Maybe
                                                                      BillingGroupMetadata),
                                                                  _dscrbbllnggrprsResponseStatus
                                                                  :: !Int}
                                      deriving (Eq, Read, Show, Data, Typeable,
                                                Generic)

-- | Creates a value of 'DescribeBillingGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscrbbllnggrprsBillingGroupARN' - The ARN of the billing group.
--
-- * 'dscrbbllnggrprsVersion' - The version of the billing group.
--
-- * 'dscrbbllnggrprsBillingGroupProperties' - The properties of the billing group.
--
-- * 'dscrbbllnggrprsBillingGroupName' - The name of the billing group.
--
-- * 'dscrbbllnggrprsBillingGroupId' - The ID of the billing group.
--
-- * 'dscrbbllnggrprsBillingGroupMetadata' - Additional information about the billing group.
--
-- * 'dscrbbllnggrprsResponseStatus' - -- | The response status code.
describeBillingGroupResponse
    :: Int -- ^ 'dscrbbllnggrprsResponseStatus'
    -> DescribeBillingGroupResponse
describeBillingGroupResponse pResponseStatus_
  = DescribeBillingGroupResponse'{_dscrbbllnggrprsBillingGroupARN
                                    = Nothing,
                                  _dscrbbllnggrprsVersion = Nothing,
                                  _dscrbbllnggrprsBillingGroupProperties =
                                    Nothing,
                                  _dscrbbllnggrprsBillingGroupName = Nothing,
                                  _dscrbbllnggrprsBillingGroupId = Nothing,
                                  _dscrbbllnggrprsBillingGroupMetadata =
                                    Nothing,
                                  _dscrbbllnggrprsResponseStatus =
                                    pResponseStatus_}

-- | The ARN of the billing group.
dscrbbllnggrprsBillingGroupARN :: Lens' DescribeBillingGroupResponse (Maybe Text)
dscrbbllnggrprsBillingGroupARN = lens _dscrbbllnggrprsBillingGroupARN (\ s a -> s{_dscrbbllnggrprsBillingGroupARN = a})

-- | The version of the billing group.
dscrbbllnggrprsVersion :: Lens' DescribeBillingGroupResponse (Maybe Integer)
dscrbbllnggrprsVersion = lens _dscrbbllnggrprsVersion (\ s a -> s{_dscrbbllnggrprsVersion = a})

-- | The properties of the billing group.
dscrbbllnggrprsBillingGroupProperties :: Lens' DescribeBillingGroupResponse (Maybe BillingGroupProperties)
dscrbbllnggrprsBillingGroupProperties = lens _dscrbbllnggrprsBillingGroupProperties (\ s a -> s{_dscrbbllnggrprsBillingGroupProperties = a})

-- | The name of the billing group.
dscrbbllnggrprsBillingGroupName :: Lens' DescribeBillingGroupResponse (Maybe Text)
dscrbbllnggrprsBillingGroupName = lens _dscrbbllnggrprsBillingGroupName (\ s a -> s{_dscrbbllnggrprsBillingGroupName = a})

-- | The ID of the billing group.
dscrbbllnggrprsBillingGroupId :: Lens' DescribeBillingGroupResponse (Maybe Text)
dscrbbllnggrprsBillingGroupId = lens _dscrbbllnggrprsBillingGroupId (\ s a -> s{_dscrbbllnggrprsBillingGroupId = a})

-- | Additional information about the billing group.
dscrbbllnggrprsBillingGroupMetadata :: Lens' DescribeBillingGroupResponse (Maybe BillingGroupMetadata)
dscrbbllnggrprsBillingGroupMetadata = lens _dscrbbllnggrprsBillingGroupMetadata (\ s a -> s{_dscrbbllnggrprsBillingGroupMetadata = a})

-- | -- | The response status code.
dscrbbllnggrprsResponseStatus :: Lens' DescribeBillingGroupResponse Int
dscrbbllnggrprsResponseStatus = lens _dscrbbllnggrprsResponseStatus (\ s a -> s{_dscrbbllnggrprsResponseStatus = a})

instance NFData DescribeBillingGroupResponse where
