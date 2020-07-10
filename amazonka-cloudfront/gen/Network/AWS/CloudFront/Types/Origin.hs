{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Origin
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.Origin where

import Network.AWS.CloudFront.Types.CustomHeaders
import Network.AWS.CloudFront.Types.CustomOriginConfig
import Network.AWS.CloudFront.Types.S3OriginConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that describes the Amazon S3 bucket, HTTP server (for example, a web server), Amazon MediaStore, or other server from which CloudFront gets your files. This can also be an origin group, if you've created an origin group. You must specify at least one origin or origin group.
--
--
-- For the current limit on the number of origins or origin groups that you can specify for a distribution, see <https://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_cloudfront Amazon CloudFront Limits> in the /AWS General Reference/ .
--
--
-- /See:/ 'origin' smart constructor.
data Origin = Origin'{_oCustomHeaders ::
                      !(Maybe CustomHeaders),
                      _oCustomOriginConfig :: !(Maybe CustomOriginConfig),
                      _oS3OriginConfig :: !(Maybe S3OriginConfig),
                      _oOriginPath :: !(Maybe Text), _oId :: !Text,
                      _oDomainName :: !Text}
                deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Origin' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oCustomHeaders' - A complex type that contains names and values for the custom headers that you want.
--
-- * 'oCustomOriginConfig' - A complex type that contains information about a custom origin. If the origin is an Amazon S3 bucket, use the @S3OriginConfig@ element instead.
--
-- * 'oS3OriginConfig' - A complex type that contains information about the Amazon S3 origin. If the origin is a custom origin, use the @CustomOriginConfig@ element instead.
--
-- * 'oOriginPath' - An optional element that causes CloudFront to request your content from a directory in your Amazon S3 bucket or your custom origin. When you include the @OriginPath@ element, specify the directory name, beginning with a @/@ . CloudFront appends the directory name to the value of @DomainName@ , for example, @example.com/production@ . Do not include a @/@ at the end of the directory name. For example, suppose you've specified the following values for your distribution:     * @DomainName@ : An Amazon S3 bucket named @myawsbucket@ .     * @OriginPath@ : @/production@      * @CNAME@ : @example.com@  When a user enters @example.com/index.html@ in a browser, CloudFront sends a request to Amazon S3 for @myawsbucket/production/index.html@ . When a user enters @example.com/acme/index.html@ in a browser, CloudFront sends a request to Amazon S3 for @myawsbucket/production/acme/index.html@ .
--
-- * 'oId' - A unique identifier for the origin or origin group. The value of @Id@ must be unique within the distribution. When you specify the value of @TargetOriginId@ for the default cache behavior or for another cache behavior, you indicate the origin to which you want the cache behavior to route requests by specifying the value of the @Id@ element for that origin. When a request matches the path pattern for that cache behavior, CloudFront routes the request to the specified origin. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesCacheBehavior Cache Behavior Settings> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'oDomainName' - __Amazon S3 origins__ : The DNS name of the Amazon S3 bucket from which you want CloudFront to get objects for this origin, for example, @myawsbucket.s3.amazonaws.com@ . If you set up your bucket to be configured as a website endpoint, enter the Amazon S3 static website hosting endpoint for the bucket. For more information about specifying this value for different types of origins, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesDomainName Origin Domain Name> in the /Amazon CloudFront Developer Guide/ . Constraints for Amazon S3 origins:      * If you configured Amazon S3 Transfer Acceleration for your bucket, don't specify the @s3-accelerate@ endpoint for @DomainName@ .     * The bucket name must be between 3 and 63 characters long (inclusive).     * The bucket name must contain only lowercase characters, numbers, periods, underscores, and dashes.     * The bucket name must not contain adjacent periods. __Custom Origins__ : The DNS domain name for the HTTP server from which you want CloudFront to get objects for this origin, for example, @www.example.com@ .  Constraints for custom origins:     * @DomainName@ must be a valid DNS name that contains only a-z, A-Z, 0-9, dot (.), hyphen (-), or underscore (_) characters.     * The name cannot exceed 128 characters.
origin
    :: Text -- ^ 'oId'
    -> Text -- ^ 'oDomainName'
    -> Origin
origin pId_ pDomainName_
  = Origin'{_oCustomHeaders = Nothing,
            _oCustomOriginConfig = Nothing,
            _oS3OriginConfig = Nothing, _oOriginPath = Nothing,
            _oId = pId_, _oDomainName = pDomainName_}

-- | A complex type that contains names and values for the custom headers that you want.
oCustomHeaders :: Lens' Origin (Maybe CustomHeaders)
oCustomHeaders = lens _oCustomHeaders (\ s a -> s{_oCustomHeaders = a})

-- | A complex type that contains information about a custom origin. If the origin is an Amazon S3 bucket, use the @S3OriginConfig@ element instead.
oCustomOriginConfig :: Lens' Origin (Maybe CustomOriginConfig)
oCustomOriginConfig = lens _oCustomOriginConfig (\ s a -> s{_oCustomOriginConfig = a})

-- | A complex type that contains information about the Amazon S3 origin. If the origin is a custom origin, use the @CustomOriginConfig@ element instead.
oS3OriginConfig :: Lens' Origin (Maybe S3OriginConfig)
oS3OriginConfig = lens _oS3OriginConfig (\ s a -> s{_oS3OriginConfig = a})

-- | An optional element that causes CloudFront to request your content from a directory in your Amazon S3 bucket or your custom origin. When you include the @OriginPath@ element, specify the directory name, beginning with a @/@ . CloudFront appends the directory name to the value of @DomainName@ , for example, @example.com/production@ . Do not include a @/@ at the end of the directory name. For example, suppose you've specified the following values for your distribution:     * @DomainName@ : An Amazon S3 bucket named @myawsbucket@ .     * @OriginPath@ : @/production@      * @CNAME@ : @example.com@  When a user enters @example.com/index.html@ in a browser, CloudFront sends a request to Amazon S3 for @myawsbucket/production/index.html@ . When a user enters @example.com/acme/index.html@ in a browser, CloudFront sends a request to Amazon S3 for @myawsbucket/production/acme/index.html@ .
oOriginPath :: Lens' Origin (Maybe Text)
oOriginPath = lens _oOriginPath (\ s a -> s{_oOriginPath = a})

-- | A unique identifier for the origin or origin group. The value of @Id@ must be unique within the distribution. When you specify the value of @TargetOriginId@ for the default cache behavior or for another cache behavior, you indicate the origin to which you want the cache behavior to route requests by specifying the value of the @Id@ element for that origin. When a request matches the path pattern for that cache behavior, CloudFront routes the request to the specified origin. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesCacheBehavior Cache Behavior Settings> in the /Amazon CloudFront Developer Guide/ .
oId :: Lens' Origin Text
oId = lens _oId (\ s a -> s{_oId = a})

-- | __Amazon S3 origins__ : The DNS name of the Amazon S3 bucket from which you want CloudFront to get objects for this origin, for example, @myawsbucket.s3.amazonaws.com@ . If you set up your bucket to be configured as a website endpoint, enter the Amazon S3 static website hosting endpoint for the bucket. For more information about specifying this value for different types of origins, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesDomainName Origin Domain Name> in the /Amazon CloudFront Developer Guide/ . Constraints for Amazon S3 origins:      * If you configured Amazon S3 Transfer Acceleration for your bucket, don't specify the @s3-accelerate@ endpoint for @DomainName@ .     * The bucket name must be between 3 and 63 characters long (inclusive).     * The bucket name must contain only lowercase characters, numbers, periods, underscores, and dashes.     * The bucket name must not contain adjacent periods. __Custom Origins__ : The DNS domain name for the HTTP server from which you want CloudFront to get objects for this origin, for example, @www.example.com@ .  Constraints for custom origins:     * @DomainName@ must be a valid DNS name that contains only a-z, A-Z, 0-9, dot (.), hyphen (-), or underscore (_) characters.     * The name cannot exceed 128 characters.
oDomainName :: Lens' Origin Text
oDomainName = lens _oDomainName (\ s a -> s{_oDomainName = a})

instance FromXML Origin where
        parseXML x
          = Origin' <$>
              (x .@? "CustomHeaders") <*>
                (x .@? "CustomOriginConfig")
                <*> (x .@? "S3OriginConfig")
                <*> (x .@? "OriginPath")
                <*> (x .@ "Id")
                <*> (x .@ "DomainName")

instance Hashable Origin where

instance NFData Origin where

instance ToXML Origin where
        toXML Origin'{..}
          = mconcat
              ["CustomHeaders" @= _oCustomHeaders,
               "CustomOriginConfig" @= _oCustomOriginConfig,
               "S3OriginConfig" @= _oS3OriginConfig,
               "OriginPath" @= _oOriginPath, "Id" @= _oId,
               "DomainName" @= _oDomainName]
