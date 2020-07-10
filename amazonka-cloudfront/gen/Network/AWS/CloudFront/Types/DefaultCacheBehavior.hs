{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.DefaultCacheBehavior
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.DefaultCacheBehavior where

import Network.AWS.CloudFront.Types.AllowedMethods
import Network.AWS.CloudFront.Types.ForwardedValues
import Network.AWS.CloudFront.Types.LambdaFunctionAssociations
import Network.AWS.CloudFront.Types.TrustedSigners
import Network.AWS.CloudFront.Types.ViewerProtocolPolicy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that describes the default cache behavior if you don't specify a @CacheBehavior@ element or if files don't match any of the values of @PathPattern@ in @CacheBehavior@ elements. You must create exactly one default cache behavior.
--
--
--
-- /See:/ 'defaultCacheBehavior' smart constructor.
data DefaultCacheBehavior = DefaultCacheBehavior'{_dcbAllowedMethods
                                                  :: !(Maybe AllowedMethods),
                                                  _dcbLambdaFunctionAssociations
                                                  ::
                                                  !(Maybe
                                                      LambdaFunctionAssociations),
                                                  _dcbMaxTTL ::
                                                  !(Maybe Integer),
                                                  _dcbCompress :: !(Maybe Bool),
                                                  _dcbSmoothStreaming ::
                                                  !(Maybe Bool),
                                                  _dcbDefaultTTL ::
                                                  !(Maybe Integer),
                                                  _dcbFieldLevelEncryptionId ::
                                                  !(Maybe Text),
                                                  _dcbTargetOriginId :: !Text,
                                                  _dcbForwardedValues ::
                                                  !ForwardedValues,
                                                  _dcbTrustedSigners ::
                                                  !TrustedSigners,
                                                  _dcbViewerProtocolPolicy ::
                                                  !ViewerProtocolPolicy,
                                                  _dcbMinTTL :: !Integer}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DefaultCacheBehavior' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcbAllowedMethods' - Undocumented member.
--
-- * 'dcbLambdaFunctionAssociations' - A complex type that contains zero or more Lambda function associations for a cache behavior.
--
-- * 'dcbMaxTTL' - The maximum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin adds HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'dcbCompress' - Whether you want CloudFront to automatically compress certain files for this cache behavior. If so, specify @true@ ; if not, specify @false@ . For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/ServingCompressedFiles.html Serving Compressed Files> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'dcbSmoothStreaming' - Indicates whether you want to distribute media files in the Microsoft Smooth Streaming format using the origin that is associated with this cache behavior. If so, specify @true@ ; if not, specify @false@ . If you specify @true@ for @SmoothStreaming@ , you can still distribute other content using this cache behavior if the content matches the value of @PathPattern@ . 
--
-- * 'dcbDefaultTTL' - The default amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin does not add HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'dcbFieldLevelEncryptionId' - The value of @ID@ for the field-level encryption configuration that you want CloudFront to use for encrypting specific fields of data for a cache behavior or for the default cache behavior in your distribution.
--
-- * 'dcbTargetOriginId' - The value of @ID@ for the origin that you want CloudFront to route requests to when a request matches the path pattern either for a cache behavior or for the default cache behavior in your distribution.
--
-- * 'dcbForwardedValues' - A complex type that specifies how CloudFront handles query strings, cookies, and HTTP headers.
--
-- * 'dcbTrustedSigners' - A complex type that specifies the AWS accounts, if any, that you want to allow to create signed URLs for private content. If you want to require signed URLs in requests for objects in the target origin that match the @PathPattern@ for this cache behavior, specify @true@ for @Enabled@ , and specify the applicable values for @Quantity@ and @Items@ . For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ . If you don't want to require signed URLs in requests for objects that match @PathPattern@ , specify @false@ for @Enabled@ and @0@ for @Quantity@ . Omit @Items@ . To add, change, or remove one or more trusted signers, change @Enabled@ to @true@ (if it's currently @false@ ), change @Quantity@ as applicable, and specify all of the trusted signers that you want to include in the updated distribution.
--
-- * 'dcbViewerProtocolPolicy' - The protocol that viewers can use to access the files in the origin specified by @TargetOriginId@ when a request matches the path pattern in @PathPattern@ . You can specify the following options:     * @allow-all@ : Viewers can use HTTP or HTTPS.     * @redirect-to-https@ : If a viewer submits an HTTP request, CloudFront returns an HTTP status code of 301 (Moved Permanently) to the viewer along with the HTTPS URL. The viewer then resubmits the request using the new URL.     * @https-only@ : If a viewer sends an HTTP request, CloudFront returns an HTTP status code of 403 (Forbidden). For more information about requiring the HTTPS protocol, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/SecureConnections.html Using an HTTPS Connection to Access Your Objects> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'dcbMinTTL' - The minimum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ . You must specify @0@ for @MinTTL@ if you configure CloudFront to forward all headers to your origin (under @Headers@ , if you specify @1@ for @Quantity@ and @*@ for @Name@ ).
defaultCacheBehavior
    :: Text -- ^ 'dcbTargetOriginId'
    -> ForwardedValues -- ^ 'dcbForwardedValues'
    -> TrustedSigners -- ^ 'dcbTrustedSigners'
    -> ViewerProtocolPolicy -- ^ 'dcbViewerProtocolPolicy'
    -> Integer -- ^ 'dcbMinTTL'
    -> DefaultCacheBehavior
defaultCacheBehavior pTargetOriginId_
  pForwardedValues_ pTrustedSigners_
  pViewerProtocolPolicy_ pMinTTL_
  = DefaultCacheBehavior'{_dcbAllowedMethods = Nothing,
                          _dcbLambdaFunctionAssociations = Nothing,
                          _dcbMaxTTL = Nothing, _dcbCompress = Nothing,
                          _dcbSmoothStreaming = Nothing,
                          _dcbDefaultTTL = Nothing,
                          _dcbFieldLevelEncryptionId = Nothing,
                          _dcbTargetOriginId = pTargetOriginId_,
                          _dcbForwardedValues = pForwardedValues_,
                          _dcbTrustedSigners = pTrustedSigners_,
                          _dcbViewerProtocolPolicy = pViewerProtocolPolicy_,
                          _dcbMinTTL = pMinTTL_}

-- | Undocumented member.
dcbAllowedMethods :: Lens' DefaultCacheBehavior (Maybe AllowedMethods)
dcbAllowedMethods = lens _dcbAllowedMethods (\ s a -> s{_dcbAllowedMethods = a})

-- | A complex type that contains zero or more Lambda function associations for a cache behavior.
dcbLambdaFunctionAssociations :: Lens' DefaultCacheBehavior (Maybe LambdaFunctionAssociations)
dcbLambdaFunctionAssociations = lens _dcbLambdaFunctionAssociations (\ s a -> s{_dcbLambdaFunctionAssociations = a})

-- | The maximum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin adds HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
dcbMaxTTL :: Lens' DefaultCacheBehavior (Maybe Integer)
dcbMaxTTL = lens _dcbMaxTTL (\ s a -> s{_dcbMaxTTL = a})

-- | Whether you want CloudFront to automatically compress certain files for this cache behavior. If so, specify @true@ ; if not, specify @false@ . For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/ServingCompressedFiles.html Serving Compressed Files> in the /Amazon CloudFront Developer Guide/ .
dcbCompress :: Lens' DefaultCacheBehavior (Maybe Bool)
dcbCompress = lens _dcbCompress (\ s a -> s{_dcbCompress = a})

-- | Indicates whether you want to distribute media files in the Microsoft Smooth Streaming format using the origin that is associated with this cache behavior. If so, specify @true@ ; if not, specify @false@ . If you specify @true@ for @SmoothStreaming@ , you can still distribute other content using this cache behavior if the content matches the value of @PathPattern@ . 
dcbSmoothStreaming :: Lens' DefaultCacheBehavior (Maybe Bool)
dcbSmoothStreaming = lens _dcbSmoothStreaming (\ s a -> s{_dcbSmoothStreaming = a})

-- | The default amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin does not add HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
dcbDefaultTTL :: Lens' DefaultCacheBehavior (Maybe Integer)
dcbDefaultTTL = lens _dcbDefaultTTL (\ s a -> s{_dcbDefaultTTL = a})

-- | The value of @ID@ for the field-level encryption configuration that you want CloudFront to use for encrypting specific fields of data for a cache behavior or for the default cache behavior in your distribution.
dcbFieldLevelEncryptionId :: Lens' DefaultCacheBehavior (Maybe Text)
dcbFieldLevelEncryptionId = lens _dcbFieldLevelEncryptionId (\ s a -> s{_dcbFieldLevelEncryptionId = a})

-- | The value of @ID@ for the origin that you want CloudFront to route requests to when a request matches the path pattern either for a cache behavior or for the default cache behavior in your distribution.
dcbTargetOriginId :: Lens' DefaultCacheBehavior Text
dcbTargetOriginId = lens _dcbTargetOriginId (\ s a -> s{_dcbTargetOriginId = a})

-- | A complex type that specifies how CloudFront handles query strings, cookies, and HTTP headers.
dcbForwardedValues :: Lens' DefaultCacheBehavior ForwardedValues
dcbForwardedValues = lens _dcbForwardedValues (\ s a -> s{_dcbForwardedValues = a})

-- | A complex type that specifies the AWS accounts, if any, that you want to allow to create signed URLs for private content. If you want to require signed URLs in requests for objects in the target origin that match the @PathPattern@ for this cache behavior, specify @true@ for @Enabled@ , and specify the applicable values for @Quantity@ and @Items@ . For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ . If you don't want to require signed URLs in requests for objects that match @PathPattern@ , specify @false@ for @Enabled@ and @0@ for @Quantity@ . Omit @Items@ . To add, change, or remove one or more trusted signers, change @Enabled@ to @true@ (if it's currently @false@ ), change @Quantity@ as applicable, and specify all of the trusted signers that you want to include in the updated distribution.
dcbTrustedSigners :: Lens' DefaultCacheBehavior TrustedSigners
dcbTrustedSigners = lens _dcbTrustedSigners (\ s a -> s{_dcbTrustedSigners = a})

-- | The protocol that viewers can use to access the files in the origin specified by @TargetOriginId@ when a request matches the path pattern in @PathPattern@ . You can specify the following options:     * @allow-all@ : Viewers can use HTTP or HTTPS.     * @redirect-to-https@ : If a viewer submits an HTTP request, CloudFront returns an HTTP status code of 301 (Moved Permanently) to the viewer along with the HTTPS URL. The viewer then resubmits the request using the new URL.     * @https-only@ : If a viewer sends an HTTP request, CloudFront returns an HTTP status code of 403 (Forbidden). For more information about requiring the HTTPS protocol, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/SecureConnections.html Using an HTTPS Connection to Access Your Objects> in the /Amazon CloudFront Developer Guide/ .
dcbViewerProtocolPolicy :: Lens' DefaultCacheBehavior ViewerProtocolPolicy
dcbViewerProtocolPolicy = lens _dcbViewerProtocolPolicy (\ s a -> s{_dcbViewerProtocolPolicy = a})

-- | The minimum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ . You must specify @0@ for @MinTTL@ if you configure CloudFront to forward all headers to your origin (under @Headers@ , if you specify @1@ for @Quantity@ and @*@ for @Name@ ).
dcbMinTTL :: Lens' DefaultCacheBehavior Integer
dcbMinTTL = lens _dcbMinTTL (\ s a -> s{_dcbMinTTL = a})

instance FromXML DefaultCacheBehavior where
        parseXML x
          = DefaultCacheBehavior' <$>
              (x .@? "AllowedMethods") <*>
                (x .@? "LambdaFunctionAssociations")
                <*> (x .@? "MaxTTL")
                <*> (x .@? "Compress")
                <*> (x .@? "SmoothStreaming")
                <*> (x .@? "DefaultTTL")
                <*> (x .@? "FieldLevelEncryptionId")
                <*> (x .@ "TargetOriginId")
                <*> (x .@ "ForwardedValues")
                <*> (x .@ "TrustedSigners")
                <*> (x .@ "ViewerProtocolPolicy")
                <*> (x .@ "MinTTL")

instance Hashable DefaultCacheBehavior where

instance NFData DefaultCacheBehavior where

instance ToXML DefaultCacheBehavior where
        toXML DefaultCacheBehavior'{..}
          = mconcat
              ["AllowedMethods" @= _dcbAllowedMethods,
               "LambdaFunctionAssociations" @=
                 _dcbLambdaFunctionAssociations,
               "MaxTTL" @= _dcbMaxTTL, "Compress" @= _dcbCompress,
               "SmoothStreaming" @= _dcbSmoothStreaming,
               "DefaultTTL" @= _dcbDefaultTTL,
               "FieldLevelEncryptionId" @=
                 _dcbFieldLevelEncryptionId,
               "TargetOriginId" @= _dcbTargetOriginId,
               "ForwardedValues" @= _dcbForwardedValues,
               "TrustedSigners" @= _dcbTrustedSigners,
               "ViewerProtocolPolicy" @= _dcbViewerProtocolPolicy,
               "MinTTL" @= _dcbMinTTL]
