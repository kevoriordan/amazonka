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
-- Module      : Network.AWS.EC2.DescribeImages
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified images (AMIs, AKIs, and ARIs) available to you or all of the images available to you.
--
--
-- The images available to you include public images, private images that you own, and private images owned by other AWS accounts for which you have explicit launch permissions.
--
-- Recently deregistered images appear in the returned results for a short interval and then return empty results. After all instances that reference a deregistered AMI are terminated, specifying the ID of the image results in an error indicating that the AMI ID cannot be found.
--
module Network.AWS.EC2.DescribeImages
    (
    -- * Creating a Request
      describeImages
    , DescribeImages
    -- * Request Lenses
    , dscrbimgsOwners
    , dscrbimgsExecutableUsers
    , dscrbimgsFilters
    , dscrbimgsImageIds
    , dscrbimgsDryRun

    -- * Destructuring the Response
    , describeImagesResponse
    , DescribeImagesResponse
    -- * Response Lenses
    , dscrbimgsrsImages
    , dscrbimgsrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeImages' smart constructor.
data DescribeImages = DescribeImages'{_dscrbimgsOwners
                                      :: !(Maybe [Text]),
                                      _dscrbimgsExecutableUsers ::
                                      !(Maybe [Text]),
                                      _dscrbimgsFilters :: !(Maybe [Filter]),
                                      _dscrbimgsImageIds :: !(Maybe [Text]),
                                      _dscrbimgsDryRun :: !(Maybe Bool)}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeImages' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscrbimgsOwners' - Filters the images by the owner. Specify an AWS account ID, @self@ (owner is the sender of the request), or an AWS owner alias (valid values are @amazon@ | @aws-marketplace@ | @microsoft@ ). Omitting this option returns all images for which you have launch permissions, regardless of ownership.
--
-- * 'dscrbimgsExecutableUsers' - Scopes the images by users with explicit launch permissions. Specify an AWS account ID, @self@ (the sender of the request), or @all@ (public AMIs).
--
-- * 'dscrbimgsFilters' - The filters.     * @architecture@ - The image architecture (@i386@ | @x86_64@ | @arm64@ ).     * @block-device-mapping.delete-on-termination@ - A Boolean value that indicates whether the Amazon EBS volume is deleted on instance termination.     * @block-device-mapping.device-name@ - The device name specified in the block device mapping (for example, @/dev/sdh@ or @xvdh@ ).     * @block-device-mapping.snapshot-id@ - The ID of the snapshot used for the EBS volume.     * @block-device-mapping.volume-size@ - The volume size of the EBS volume, in GiB.     * @block-device-mapping.volume-type@ - The volume type of the EBS volume (@gp2@ | @io1@ | @st1 @ | @sc1@ | @standard@ ).     * @block-device-mapping.encrypted@ - A Boolean that indicates whether the EBS volume is encrypted.     * @description@ - The description of the image (provided during image creation).     * @ena-support@ - A Boolean that indicates whether enhanced networking with ENA is enabled.     * @hypervisor@ - The hypervisor type (@ovm@ | @xen@ ).     * @image-id@ - The ID of the image.     * @image-type@ - The image type (@machine@ | @kernel@ | @ramdisk@ ).     * @is-public@ - A Boolean that indicates whether the image is public.     * @kernel-id@ - The kernel ID.     * @manifest-location@ - The location of the image manifest.     * @name@ - The name of the AMI (provided during image creation).     * @owner-alias@ - String value from an Amazon-maintained list (@amazon@ | @aws-marketplace@ | @microsoft@ ) of snapshot owners. Not to be confused with the user-configured AWS account alias, which is set from the IAM console.     * @owner-id@ - The AWS account ID of the image owner.     * @platform@ - The platform. To only list Windows-based AMIs, use @windows@ .     * @product-code@ - The product code.     * @product-code.type@ - The type of the product code (@devpay@ | @marketplace@ ).     * @ramdisk-id@ - The RAM disk ID.     * @root-device-name@ - The device name of the root device volume (for example, @/dev/sda1@ ).     * @root-device-type@ - The type of the root device volume (@ebs@ | @instance-store@ ).     * @state@ - The state of the image (@available@ | @pending@ | @failed@ ).     * @state-reason-code@ - The reason code for the state change.     * @state-reason-message@ - The message for the state change.     * @sriov-net-support@ - A value of @simple@ indicates that enhanced networking with the Intel 82599 VF interface is enabled.     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.     * @virtualization-type@ - The virtualization type (@paravirtual@ | @hvm@ ).
--
-- * 'dscrbimgsImageIds' - The image IDs. Default: Describes all images available to you.
--
-- * 'dscrbimgsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
describeImages
    :: DescribeImages
describeImages
  = DescribeImages'{_dscrbimgsOwners = Nothing,
                    _dscrbimgsExecutableUsers = Nothing,
                    _dscrbimgsFilters = Nothing,
                    _dscrbimgsImageIds = Nothing,
                    _dscrbimgsDryRun = Nothing}

-- | Filters the images by the owner. Specify an AWS account ID, @self@ (owner is the sender of the request), or an AWS owner alias (valid values are @amazon@ | @aws-marketplace@ | @microsoft@ ). Omitting this option returns all images for which you have launch permissions, regardless of ownership.
dscrbimgsOwners :: Lens' DescribeImages [Text]
dscrbimgsOwners = lens _dscrbimgsOwners (\ s a -> s{_dscrbimgsOwners = a}) . _Default . _Coerce

-- | Scopes the images by users with explicit launch permissions. Specify an AWS account ID, @self@ (the sender of the request), or @all@ (public AMIs).
dscrbimgsExecutableUsers :: Lens' DescribeImages [Text]
dscrbimgsExecutableUsers = lens _dscrbimgsExecutableUsers (\ s a -> s{_dscrbimgsExecutableUsers = a}) . _Default . _Coerce

-- | The filters.     * @architecture@ - The image architecture (@i386@ | @x86_64@ | @arm64@ ).     * @block-device-mapping.delete-on-termination@ - A Boolean value that indicates whether the Amazon EBS volume is deleted on instance termination.     * @block-device-mapping.device-name@ - The device name specified in the block device mapping (for example, @/dev/sdh@ or @xvdh@ ).     * @block-device-mapping.snapshot-id@ - The ID of the snapshot used for the EBS volume.     * @block-device-mapping.volume-size@ - The volume size of the EBS volume, in GiB.     * @block-device-mapping.volume-type@ - The volume type of the EBS volume (@gp2@ | @io1@ | @st1 @ | @sc1@ | @standard@ ).     * @block-device-mapping.encrypted@ - A Boolean that indicates whether the EBS volume is encrypted.     * @description@ - The description of the image (provided during image creation).     * @ena-support@ - A Boolean that indicates whether enhanced networking with ENA is enabled.     * @hypervisor@ - The hypervisor type (@ovm@ | @xen@ ).     * @image-id@ - The ID of the image.     * @image-type@ - The image type (@machine@ | @kernel@ | @ramdisk@ ).     * @is-public@ - A Boolean that indicates whether the image is public.     * @kernel-id@ - The kernel ID.     * @manifest-location@ - The location of the image manifest.     * @name@ - The name of the AMI (provided during image creation).     * @owner-alias@ - String value from an Amazon-maintained list (@amazon@ | @aws-marketplace@ | @microsoft@ ) of snapshot owners. Not to be confused with the user-configured AWS account alias, which is set from the IAM console.     * @owner-id@ - The AWS account ID of the image owner.     * @platform@ - The platform. To only list Windows-based AMIs, use @windows@ .     * @product-code@ - The product code.     * @product-code.type@ - The type of the product code (@devpay@ | @marketplace@ ).     * @ramdisk-id@ - The RAM disk ID.     * @root-device-name@ - The device name of the root device volume (for example, @/dev/sda1@ ).     * @root-device-type@ - The type of the root device volume (@ebs@ | @instance-store@ ).     * @state@ - The state of the image (@available@ | @pending@ | @failed@ ).     * @state-reason-code@ - The reason code for the state change.     * @state-reason-message@ - The message for the state change.     * @sriov-net-support@ - A value of @simple@ indicates that enhanced networking with the Intel 82599 VF interface is enabled.     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.     * @virtualization-type@ - The virtualization type (@paravirtual@ | @hvm@ ).
dscrbimgsFilters :: Lens' DescribeImages [Filter]
dscrbimgsFilters = lens _dscrbimgsFilters (\ s a -> s{_dscrbimgsFilters = a}) . _Default . _Coerce

-- | The image IDs. Default: Describes all images available to you.
dscrbimgsImageIds :: Lens' DescribeImages [Text]
dscrbimgsImageIds = lens _dscrbimgsImageIds (\ s a -> s{_dscrbimgsImageIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dscrbimgsDryRun :: Lens' DescribeImages (Maybe Bool)
dscrbimgsDryRun = lens _dscrbimgsDryRun (\ s a -> s{_dscrbimgsDryRun = a})

instance AWSRequest DescribeImages where
        type Rs DescribeImages = DescribeImagesResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeImagesResponse' <$>
                   (x .@? "imagesSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeImages where

instance NFData DescribeImages where

instance ToHeaders DescribeImages where
        toHeaders = const mempty

instance ToPath DescribeImages where
        toPath = const "/"

instance ToQuery DescribeImages where
        toQuery DescribeImages'{..}
          = mconcat
              ["Action" =: ("DescribeImages" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Owner" <$> _dscrbimgsOwners),
               toQuery
                 (toQueryList "ExecutableBy" <$>
                    _dscrbimgsExecutableUsers),
               toQuery (toQueryList "Filter" <$> _dscrbimgsFilters),
               toQuery
                 (toQueryList "ImageId" <$> _dscrbimgsImageIds),
               "DryRun" =: _dscrbimgsDryRun]

-- | /See:/ 'describeImagesResponse' smart constructor.
data DescribeImagesResponse = DescribeImagesResponse'{_dscrbimgsrsImages
                                                      :: !(Maybe [Image]),
                                                      _dscrbimgsrsResponseStatus
                                                      :: !Int}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'DescribeImagesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscrbimgsrsImages' - Information about the images.
--
-- * 'dscrbimgsrsResponseStatus' - -- | The response status code.
describeImagesResponse
    :: Int -- ^ 'dscrbimgsrsResponseStatus'
    -> DescribeImagesResponse
describeImagesResponse pResponseStatus_
  = DescribeImagesResponse'{_dscrbimgsrsImages =
                              Nothing,
                            _dscrbimgsrsResponseStatus = pResponseStatus_}

-- | Information about the images.
dscrbimgsrsImages :: Lens' DescribeImagesResponse [Image]
dscrbimgsrsImages = lens _dscrbimgsrsImages (\ s a -> s{_dscrbimgsrsImages = a}) . _Default . _Coerce

-- | -- | The response status code.
dscrbimgsrsResponseStatus :: Lens' DescribeImagesResponse Int
dscrbimgsrsResponseStatus = lens _dscrbimgsrsResponseStatus (\ s a -> s{_dscrbimgsrsResponseStatus = a})

instance NFData DescribeImagesResponse where
