{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CacheBehavior
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.CacheBehavior where

import Network.AWS.CloudFront.Types.AllowedMethods
import Network.AWS.CloudFront.Types.ForwardedValues
import Network.AWS.CloudFront.Types.LambdaFunctionAssociations
import Network.AWS.CloudFront.Types.TrustedSigners
import Network.AWS.CloudFront.Types.ViewerProtocolPolicy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that describes how CloudFront processes requests.
--
--
-- You must create at least as many cache behaviors (including the default cache behavior) as you have origins if you want CloudFront to distribute objects from all of the origins. Each cache behavior specifies the one origin from which you want CloudFront to get objects. If you have two origins and only the default cache behavior, the default cache behavior will cause CloudFront to get objects from one of the origins, but the other origin is never used.
--
-- For the current limit on the number of cache behaviors that you can add to a distribution, see <https://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_cloudfront Amazon CloudFront Limits> in the /AWS General Reference/ .
--
-- If you don't want to specify any cache behaviors, include only an empty @CacheBehaviors@ element. Don't include an empty @CacheBehavior@ element, or CloudFront returns a @MalformedXML@ error.
--
-- To delete all cache behaviors in an existing distribution, update the distribution configuration and include only an empty @CacheBehaviors@ element.
--
-- To add, change, or remove one or more cache behaviors, update the distribution configuration and specify all of the cache behaviors that you want to include in the updated distribution.
--
-- For more information about cache behaviors, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesCacheBehavior Cache Behaviors> in the /Amazon CloudFront Developer Guide/ .
--
--
-- /See:/ 'cacheBehavior' smart constructor.
data CacheBehavior = CacheBehavior'{_cbAllowedMethods
                                    :: !(Maybe AllowedMethods),
                                    _cbLambdaFunctionAssociations ::
                                    !(Maybe LambdaFunctionAssociations),
                                    _cbMaxTTL :: !(Maybe Integer),
                                    _cbCompress :: !(Maybe Bool),
                                    _cbSmoothStreaming :: !(Maybe Bool),
                                    _cbDefaultTTL :: !(Maybe Integer),
                                    _cbFieldLevelEncryptionId :: !(Maybe Text),
                                    _cbPathPattern :: !Text,
                                    _cbTargetOriginId :: !Text,
                                    _cbForwardedValues :: !ForwardedValues,
                                    _cbTrustedSigners :: !TrustedSigners,
                                    _cbViewerProtocolPolicy ::
                                    !ViewerProtocolPolicy,
                                    _cbMinTTL :: !Integer}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CacheBehavior' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbAllowedMethods' - Undocumented member.
--
-- * 'cbLambdaFunctionAssociations' - A complex type that contains zero or more Lambda function associations for a cache behavior.
--
-- * 'cbMaxTTL' - The maximum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin adds HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'cbCompress' - Whether you want CloudFront to automatically compress certain files for this cache behavior. If so, specify true; if not, specify false. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/ServingCompressedFiles.html Serving Compressed Files> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'cbSmoothStreaming' - Indicates whether you want to distribute media files in the Microsoft Smooth Streaming format using the origin that is associated with this cache behavior. If so, specify @true@ ; if not, specify @false@ . If you specify @true@ for @SmoothStreaming@ , you can still distribute other content using this cache behavior if the content matches the value of @PathPattern@ . 
--
-- * 'cbDefaultTTL' - The default amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin does not add HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'cbFieldLevelEncryptionId' - The value of @ID@ for the field-level encryption configuration that you want CloudFront to use for encrypting specific fields of data for a cache behavior or for the default cache behavior in your distribution.
--
-- * 'cbPathPattern' - The pattern (for example, @images/*.jpg@ ) that specifies which requests to apply the behavior to. When CloudFront receives a viewer request, the requested path is compared with path patterns in the order in which cache behaviors are listed in the distribution. The path pattern for the default cache behavior is @*@ and cannot be changed. If the request for an object does not match the path pattern for any cache behaviors, CloudFront applies the behavior in the default cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesPathPattern Path Pattern> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'cbTargetOriginId' - The value of @ID@ for the origin that you want CloudFront to route requests to when a request matches the path pattern either for a cache behavior or for the default cache behavior in your distribution.
--
-- * 'cbForwardedValues' - A complex type that specifies how CloudFront handles query strings, cookies, and HTTP headers.
--
-- * 'cbTrustedSigners' - A complex type that specifies the AWS accounts, if any, that you want to allow to create signed URLs for private content. If you want to require signed URLs in requests for objects in the target origin that match the @PathPattern@ for this cache behavior, specify @true@ for @Enabled@ , and specify the applicable values for @Quantity@ and @Items@ . For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ .  If you don't want to require signed URLs in requests for objects that match @PathPattern@ , specify @false@ for @Enabled@ and @0@ for @Quantity@ . Omit @Items@ . To add, change, or remove one or more trusted signers, change @Enabled@ to @true@ (if it's currently @false@ ), change @Quantity@ as applicable, and specify all of the trusted signers that you want to include in the updated distribution.
--
-- * 'cbViewerProtocolPolicy' - The protocol that viewers can use to access the files in the origin specified by @TargetOriginId@ when a request matches the path pattern in @PathPattern@ . You can specify the following options:     * @allow-all@ : Viewers can use HTTP or HTTPS.     * @redirect-to-https@ : If a viewer submits an HTTP request, CloudFront returns an HTTP status code of 301 (Moved Permanently) to the viewer along with the HTTPS URL. The viewer then resubmits the request using the new URL.      * @https-only@ : If a viewer sends an HTTP request, CloudFront returns an HTTP status code of 403 (Forbidden).  For more information about requiring the HTTPS protocol, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/SecureConnections.html Using an HTTPS Connection to Access Your Objects> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'cbMinTTL' - The minimum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ . You must specify @0@ for @MinTTL@ if you configure CloudFront to forward all headers to your origin (under @Headers@ , if you specify @1@ for @Quantity@ and @*@ for @Name@ ).
cacheBehavior
    :: Text -- ^ 'cbPathPattern'
    -> Text -- ^ 'cbTargetOriginId'
    -> ForwardedValues -- ^ 'cbForwardedValues'
    -> TrustedSigners -- ^ 'cbTrustedSigners'
    -> ViewerProtocolPolicy -- ^ 'cbViewerProtocolPolicy'
    -> Integer -- ^ 'cbMinTTL'
    -> CacheBehavior
cacheBehavior pPathPattern_ pTargetOriginId_
  pForwardedValues_ pTrustedSigners_
  pViewerProtocolPolicy_ pMinTTL_
  = CacheBehavior'{_cbAllowedMethods = Nothing,
                   _cbLambdaFunctionAssociations = Nothing,
                   _cbMaxTTL = Nothing, _cbCompress = Nothing,
                   _cbSmoothStreaming = Nothing,
                   _cbDefaultTTL = Nothing,
                   _cbFieldLevelEncryptionId = Nothing,
                   _cbPathPattern = pPathPattern_,
                   _cbTargetOriginId = pTargetOriginId_,
                   _cbForwardedValues = pForwardedValues_,
                   _cbTrustedSigners = pTrustedSigners_,
                   _cbViewerProtocolPolicy = pViewerProtocolPolicy_,
                   _cbMinTTL = pMinTTL_}

-- | Undocumented member.
cbAllowedMethods :: Lens' CacheBehavior (Maybe AllowedMethods)
cbAllowedMethods = lens _cbAllowedMethods (\ s a -> s{_cbAllowedMethods = a})

-- | A complex type that contains zero or more Lambda function associations for a cache behavior.
cbLambdaFunctionAssociations :: Lens' CacheBehavior (Maybe LambdaFunctionAssociations)
cbLambdaFunctionAssociations = lens _cbLambdaFunctionAssociations (\ s a -> s{_cbLambdaFunctionAssociations = a})

-- | The maximum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin adds HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
cbMaxTTL :: Lens' CacheBehavior (Maybe Integer)
cbMaxTTL = lens _cbMaxTTL (\ s a -> s{_cbMaxTTL = a})

-- | Whether you want CloudFront to automatically compress certain files for this cache behavior. If so, specify true; if not, specify false. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/ServingCompressedFiles.html Serving Compressed Files> in the /Amazon CloudFront Developer Guide/ .
cbCompress :: Lens' CacheBehavior (Maybe Bool)
cbCompress = lens _cbCompress (\ s a -> s{_cbCompress = a})

-- | Indicates whether you want to distribute media files in the Microsoft Smooth Streaming format using the origin that is associated with this cache behavior. If so, specify @true@ ; if not, specify @false@ . If you specify @true@ for @SmoothStreaming@ , you can still distribute other content using this cache behavior if the content matches the value of @PathPattern@ . 
cbSmoothStreaming :: Lens' CacheBehavior (Maybe Bool)
cbSmoothStreaming = lens _cbSmoothStreaming (\ s a -> s{_cbSmoothStreaming = a})

-- | The default amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin does not add HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
cbDefaultTTL :: Lens' CacheBehavior (Maybe Integer)
cbDefaultTTL = lens _cbDefaultTTL (\ s a -> s{_cbDefaultTTL = a})

-- | The value of @ID@ for the field-level encryption configuration that you want CloudFront to use for encrypting specific fields of data for a cache behavior or for the default cache behavior in your distribution.
cbFieldLevelEncryptionId :: Lens' CacheBehavior (Maybe Text)
cbFieldLevelEncryptionId = lens _cbFieldLevelEncryptionId (\ s a -> s{_cbFieldLevelEncryptionId = a})

-- | The pattern (for example, @images/*.jpg@ ) that specifies which requests to apply the behavior to. When CloudFront receives a viewer request, the requested path is compared with path patterns in the order in which cache behaviors are listed in the distribution. The path pattern for the default cache behavior is @*@ and cannot be changed. If the request for an object does not match the path pattern for any cache behaviors, CloudFront applies the behavior in the default cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesPathPattern Path Pattern> in the /Amazon CloudFront Developer Guide/ .
cbPathPattern :: Lens' CacheBehavior Text
cbPathPattern = lens _cbPathPattern (\ s a -> s{_cbPathPattern = a})

-- | The value of @ID@ for the origin that you want CloudFront to route requests to when a request matches the path pattern either for a cache behavior or for the default cache behavior in your distribution.
cbTargetOriginId :: Lens' CacheBehavior Text
cbTargetOriginId = lens _cbTargetOriginId (\ s a -> s{_cbTargetOriginId = a})

-- | A complex type that specifies how CloudFront handles query strings, cookies, and HTTP headers.
cbForwardedValues :: Lens' CacheBehavior ForwardedValues
cbForwardedValues = lens _cbForwardedValues (\ s a -> s{_cbForwardedValues = a})

-- | A complex type that specifies the AWS accounts, if any, that you want to allow to create signed URLs for private content. If you want to require signed URLs in requests for objects in the target origin that match the @PathPattern@ for this cache behavior, specify @true@ for @Enabled@ , and specify the applicable values for @Quantity@ and @Items@ . For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ .  If you don't want to require signed URLs in requests for objects that match @PathPattern@ , specify @false@ for @Enabled@ and @0@ for @Quantity@ . Omit @Items@ . To add, change, or remove one or more trusted signers, change @Enabled@ to @true@ (if it's currently @false@ ), change @Quantity@ as applicable, and specify all of the trusted signers that you want to include in the updated distribution.
cbTrustedSigners :: Lens' CacheBehavior TrustedSigners
cbTrustedSigners = lens _cbTrustedSigners (\ s a -> s{_cbTrustedSigners = a})

-- | The protocol that viewers can use to access the files in the origin specified by @TargetOriginId@ when a request matches the path pattern in @PathPattern@ . You can specify the following options:     * @allow-all@ : Viewers can use HTTP or HTTPS.     * @redirect-to-https@ : If a viewer submits an HTTP request, CloudFront returns an HTTP status code of 301 (Moved Permanently) to the viewer along with the HTTPS URL. The viewer then resubmits the request using the new URL.      * @https-only@ : If a viewer sends an HTTP request, CloudFront returns an HTTP status code of 403 (Forbidden).  For more information about requiring the HTTPS protocol, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/SecureConnections.html Using an HTTPS Connection to Access Your Objects> in the /Amazon CloudFront Developer Guide/ .
cbViewerProtocolPolicy :: Lens' CacheBehavior ViewerProtocolPolicy
cbViewerProtocolPolicy = lens _cbViewerProtocolPolicy (\ s a -> s{_cbViewerProtocolPolicy = a})

-- | The minimum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ . You must specify @0@ for @MinTTL@ if you configure CloudFront to forward all headers to your origin (under @Headers@ , if you specify @1@ for @Quantity@ and @*@ for @Name@ ).
cbMinTTL :: Lens' CacheBehavior Integer
cbMinTTL = lens _cbMinTTL (\ s a -> s{_cbMinTTL = a})

instance FromXML CacheBehavior where
        parseXML x
          = CacheBehavior' <$>
              (x .@? "AllowedMethods") <*>
                (x .@? "LambdaFunctionAssociations")
                <*> (x .@? "MaxTTL")
                <*> (x .@? "Compress")
                <*> (x .@? "SmoothStreaming")
                <*> (x .@? "DefaultTTL")
                <*> (x .@? "FieldLevelEncryptionId")
                <*> (x .@ "PathPattern")
                <*> (x .@ "TargetOriginId")
                <*> (x .@ "ForwardedValues")
                <*> (x .@ "TrustedSigners")
                <*> (x .@ "ViewerProtocolPolicy")
                <*> (x .@ "MinTTL")

instance Hashable CacheBehavior where

instance NFData CacheBehavior where

instance ToXML CacheBehavior where
        toXML CacheBehavior'{..}
          = mconcat
              ["AllowedMethods" @= _cbAllowedMethods,
               "LambdaFunctionAssociations" @=
                 _cbLambdaFunctionAssociations,
               "MaxTTL" @= _cbMaxTTL, "Compress" @= _cbCompress,
               "SmoothStreaming" @= _cbSmoothStreaming,
               "DefaultTTL" @= _cbDefaultTTL,
               "FieldLevelEncryptionId" @=
                 _cbFieldLevelEncryptionId,
               "PathPattern" @= _cbPathPattern,
               "TargetOriginId" @= _cbTargetOriginId,
               "ForwardedValues" @= _cbForwardedValues,
               "TrustedSigners" @= _cbTrustedSigners,
               "ViewerProtocolPolicy" @= _cbViewerProtocolPolicy,
               "MinTTL" @= _cbMinTTL]
