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
-- Module      : Network.AWS.EC2.DescribeFpgaImages
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the Amazon FPGA Images (AFIs) available to you. These include public AFIs, private AFIs that you own, and AFIs owned by other AWS accounts for which you have load permissions.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeFpgaImages
    (
    -- * Creating a Request
      describeFpgaImages
    , DescribeFpgaImages
    -- * Request Lenses
    , dscrbfpgimgsOwners
    , dscrbfpgimgsFilters
    , dscrbfpgimgsNextToken
    , dscrbfpgimgsDryRun
    , dscrbfpgimgsMaxResults
    , dscrbfpgimgsFpgaImageIds

    -- * Destructuring the Response
    , describeFpgaImagesResponse
    , DescribeFpgaImagesResponse
    -- * Response Lenses
    , dfirsFpgaImages
    , dfirsNextToken
    , dfirsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeFpgaImages' smart constructor.
data DescribeFpgaImages = DescribeFpgaImages'{_dscrbfpgimgsOwners
                                              :: !(Maybe [Text]),
                                              _dscrbfpgimgsFilters ::
                                              !(Maybe [Filter]),
                                              _dscrbfpgimgsNextToken ::
                                              !(Maybe Text),
                                              _dscrbfpgimgsDryRun ::
                                              !(Maybe Bool),
                                              _dscrbfpgimgsMaxResults ::
                                              !(Maybe Nat),
                                              _dscrbfpgimgsFpgaImageIds ::
                                              !(Maybe [Text])}
                            deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeFpgaImages' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscrbfpgimgsOwners' - Filters the AFI by owner. Specify an AWS account ID, @self@ (owner is the sender of the request), or an AWS owner alias (valid values are @amazon@ | @aws-marketplace@ ).
--
-- * 'dscrbfpgimgsFilters' - The filters.     * @create-time@ - The creation time of the AFI.     * @fpga-image-id@ - The FPGA image identifier (AFI ID).     * @fpga-image-global-id@ - The global FPGA image identifier (AGFI ID).     * @name@ - The name of the AFI.     * @owner-id@ - The AWS account ID of the AFI owner.     * @product-code@ - The product code.     * @shell-version@ - The version of the AWS Shell that was used to create the bitstream.     * @state@ - The state of the AFI (@pending@ | @failed@ | @available@ | @unavailable@ ).     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.     * @update-time@ - The time of the most recent update.
--
-- * 'dscrbfpgimgsNextToken' - The token to retrieve the next page of results.
--
-- * 'dscrbfpgimgsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dscrbfpgimgsMaxResults' - The maximum number of results to return in a single call.
--
-- * 'dscrbfpgimgsFpgaImageIds' - The AFI IDs.
describeFpgaImages
    :: DescribeFpgaImages
describeFpgaImages
  = DescribeFpgaImages'{_dscrbfpgimgsOwners = Nothing,
                        _dscrbfpgimgsFilters = Nothing,
                        _dscrbfpgimgsNextToken = Nothing,
                        _dscrbfpgimgsDryRun = Nothing,
                        _dscrbfpgimgsMaxResults = Nothing,
                        _dscrbfpgimgsFpgaImageIds = Nothing}

-- | Filters the AFI by owner. Specify an AWS account ID, @self@ (owner is the sender of the request), or an AWS owner alias (valid values are @amazon@ | @aws-marketplace@ ).
dscrbfpgimgsOwners :: Lens' DescribeFpgaImages [Text]
dscrbfpgimgsOwners = lens _dscrbfpgimgsOwners (\ s a -> s{_dscrbfpgimgsOwners = a}) . _Default . _Coerce

-- | The filters.     * @create-time@ - The creation time of the AFI.     * @fpga-image-id@ - The FPGA image identifier (AFI ID).     * @fpga-image-global-id@ - The global FPGA image identifier (AGFI ID).     * @name@ - The name of the AFI.     * @owner-id@ - The AWS account ID of the AFI owner.     * @product-code@ - The product code.     * @shell-version@ - The version of the AWS Shell that was used to create the bitstream.     * @state@ - The state of the AFI (@pending@ | @failed@ | @available@ | @unavailable@ ).     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.     * @update-time@ - The time of the most recent update.
dscrbfpgimgsFilters :: Lens' DescribeFpgaImages [Filter]
dscrbfpgimgsFilters = lens _dscrbfpgimgsFilters (\ s a -> s{_dscrbfpgimgsFilters = a}) . _Default . _Coerce

-- | The token to retrieve the next page of results.
dscrbfpgimgsNextToken :: Lens' DescribeFpgaImages (Maybe Text)
dscrbfpgimgsNextToken = lens _dscrbfpgimgsNextToken (\ s a -> s{_dscrbfpgimgsNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dscrbfpgimgsDryRun :: Lens' DescribeFpgaImages (Maybe Bool)
dscrbfpgimgsDryRun = lens _dscrbfpgimgsDryRun (\ s a -> s{_dscrbfpgimgsDryRun = a})

-- | The maximum number of results to return in a single call.
dscrbfpgimgsMaxResults :: Lens' DescribeFpgaImages (Maybe Natural)
dscrbfpgimgsMaxResults = lens _dscrbfpgimgsMaxResults (\ s a -> s{_dscrbfpgimgsMaxResults = a}) . mapping _Nat

-- | The AFI IDs.
dscrbfpgimgsFpgaImageIds :: Lens' DescribeFpgaImages [Text]
dscrbfpgimgsFpgaImageIds = lens _dscrbfpgimgsFpgaImageIds (\ s a -> s{_dscrbfpgimgsFpgaImageIds = a}) . _Default . _Coerce

instance AWSPager DescribeFpgaImages where
        page rq rs
          | stop (rs ^. dfirsNextToken) = Nothing
          | stop (rs ^. dfirsFpgaImages) = Nothing
          | otherwise =
            Just $ rq &
              dscrbfpgimgsNextToken .~ rs ^. dfirsNextToken

instance AWSRequest DescribeFpgaImages where
        type Rs DescribeFpgaImages =
             DescribeFpgaImagesResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeFpgaImagesResponse' <$>
                   (x .@? "fpgaImageSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeFpgaImages where

instance NFData DescribeFpgaImages where

instance ToHeaders DescribeFpgaImages where
        toHeaders = const mempty

instance ToPath DescribeFpgaImages where
        toPath = const "/"

instance ToQuery DescribeFpgaImages where
        toQuery DescribeFpgaImages'{..}
          = mconcat
              ["Action" =: ("DescribeFpgaImages" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery
                 (toQueryList "Owner" <$> _dscrbfpgimgsOwners),
               toQuery
                 (toQueryList "Filter" <$> _dscrbfpgimgsFilters),
               "NextToken" =: _dscrbfpgimgsNextToken,
               "DryRun" =: _dscrbfpgimgsDryRun,
               "MaxResults" =: _dscrbfpgimgsMaxResults,
               toQuery
                 (toQueryList "FpgaImageId" <$>
                    _dscrbfpgimgsFpgaImageIds)]

-- | /See:/ 'describeFpgaImagesResponse' smart constructor.
data DescribeFpgaImagesResponse = DescribeFpgaImagesResponse'{_dfirsFpgaImages
                                                              ::
                                                              !(Maybe
                                                                  [FpgaImage]),
                                                              _dfirsNextToken ::
                                                              !(Maybe Text),
                                                              _dfirsResponseStatus
                                                              :: !Int}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'DescribeFpgaImagesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfirsFpgaImages' - Information about the FPGA images.
--
-- * 'dfirsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dfirsResponseStatus' - -- | The response status code.
describeFpgaImagesResponse
    :: Int -- ^ 'dfirsResponseStatus'
    -> DescribeFpgaImagesResponse
describeFpgaImagesResponse pResponseStatus_
  = DescribeFpgaImagesResponse'{_dfirsFpgaImages =
                                  Nothing,
                                _dfirsNextToken = Nothing,
                                _dfirsResponseStatus = pResponseStatus_}

-- | Information about the FPGA images.
dfirsFpgaImages :: Lens' DescribeFpgaImagesResponse [FpgaImage]
dfirsFpgaImages = lens _dfirsFpgaImages (\ s a -> s{_dfirsFpgaImages = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dfirsNextToken :: Lens' DescribeFpgaImagesResponse (Maybe Text)
dfirsNextToken = lens _dfirsNextToken (\ s a -> s{_dfirsNextToken = a})

-- | -- | The response status code.
dfirsResponseStatus :: Lens' DescribeFpgaImagesResponse Int
dfirsResponseStatus = lens _dfirsResponseStatus (\ s a -> s{_dfirsResponseStatus = a})

instance NFData DescribeFpgaImagesResponse where
