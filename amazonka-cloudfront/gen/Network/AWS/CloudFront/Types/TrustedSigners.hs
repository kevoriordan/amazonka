{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.TrustedSigners
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.TrustedSigners where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that specifies the AWS accounts, if any, that you want to allow to create signed URLs for private content.
--
--
-- If you want to require signed URLs in requests for objects in the target origin that match the @PathPattern@ for this cache behavior, specify @true@ for @Enabled@ , and specify the applicable values for @Quantity@ and @Items@ . For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ .
--
-- If you don't want to require signed URLs in requests for objects that match @PathPattern@ , specify @false@ for @Enabled@ and @0@ for @Quantity@ . Omit @Items@ .
--
-- To add, change, or remove one or more trusted signers, change @Enabled@ to @true@ (if it's currently @false@ ), change @Quantity@ as applicable, and specify all of the trusted signers that you want to include in the updated distribution.
--
-- For more information about updating the distribution configuration, see <https://docs.aws.amazon.com/cloudfront/latest/APIReference/DistributionConfig.html DistributionConfig> in the /Amazon CloudFront API Reference/ .
--
--
-- /See:/ 'trustedSigners' smart constructor.
data TrustedSigners = TrustedSigners'{_tsItems ::
                                      !(Maybe [Text]),
                                      _tsEnabled :: !Bool, _tsQuantity :: !Int}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrustedSigners' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsItems' - __Optional__ : A complex type that contains trusted signers for this cache behavior. If @Quantity@ is @0@ , you can omit @Items@ .
--
-- * 'tsEnabled' - Specifies whether you want to require viewers to use signed URLs to access the files specified by @PathPattern@ and @TargetOriginId@ .
--
-- * 'tsQuantity' - The number of trusted signers for this cache behavior.
trustedSigners
    :: Bool -- ^ 'tsEnabled'
    -> Int -- ^ 'tsQuantity'
    -> TrustedSigners
trustedSigners pEnabled_ pQuantity_
  = TrustedSigners'{_tsItems = Nothing,
                    _tsEnabled = pEnabled_, _tsQuantity = pQuantity_}

-- | __Optional__ : A complex type that contains trusted signers for this cache behavior. If @Quantity@ is @0@ , you can omit @Items@ .
tsItems :: Lens' TrustedSigners [Text]
tsItems = lens _tsItems (\ s a -> s{_tsItems = a}) . _Default . _Coerce

-- | Specifies whether you want to require viewers to use signed URLs to access the files specified by @PathPattern@ and @TargetOriginId@ .
tsEnabled :: Lens' TrustedSigners Bool
tsEnabled = lens _tsEnabled (\ s a -> s{_tsEnabled = a})

-- | The number of trusted signers for this cache behavior.
tsQuantity :: Lens' TrustedSigners Int
tsQuantity = lens _tsQuantity (\ s a -> s{_tsQuantity = a})

instance FromXML TrustedSigners where
        parseXML x
          = TrustedSigners' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "AwsAccountNumber"))
                <*> (x .@ "Enabled")
                <*> (x .@ "Quantity")

instance Hashable TrustedSigners where

instance NFData TrustedSigners where

instance ToXML TrustedSigners where
        toXML TrustedSigners'{..}
          = mconcat
              ["Items" @=
                 toXML (toXMLList "AwsAccountNumber" <$> _tsItems),
               "Enabled" @= _tsEnabled, "Quantity" @= _tsQuantity]
