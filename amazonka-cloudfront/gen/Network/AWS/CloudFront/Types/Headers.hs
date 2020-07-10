{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Headers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.Headers where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that specifies the request headers, if any, that you want CloudFront to base caching on for this cache behavior. 
--
--
-- For the headers that you specify, CloudFront caches separate versions of a specified object based on the header values in viewer requests. For example, suppose viewer requests for @logo.jpg@ contain a custom @product@ header that has a value of either @acme@ or @apex@ , and you configure CloudFront to cache your content based on values in the @product@ header. CloudFront forwards the @product@ header to the origin and caches the response from the origin once for each header value. For more information about caching based on header values, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/header-caching.html How CloudFront Forwards and Caches Headers> in the /Amazon CloudFront Developer Guide/ .
--
--
-- /See:/ 'headers' smart constructor.
data Headers = Headers'{_hItems :: !(Maybe [Text]),
                        _hQuantity :: !Int}
                 deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Headers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hItems' - A list that contains one @Name@ element for each header that you want CloudFront to use for caching in this cache behavior. If @Quantity@ is @0@ , omit @Items@ .
--
-- * 'hQuantity' - The number of different headers that you want CloudFront to base caching on for this cache behavior. You can configure each cache behavior in a web distribution to do one of the following:     * __Forward all headers to your origin__ : Specify @1@ for @Quantity@ and @*@ for @Name@ . /Important:/ CloudFront doesn't cache the objects that are associated with this cache behavior. Instead, CloudFront sends every request to the origin.      * __Forward a whitelist of headers you specify__ : Specify the number of headers that you want CloudFront to base caching on. Then specify the header names in @Name@ elements. CloudFront caches your objects based on the values in the specified headers.     * __Forward only the default headers__ : Specify @0@ for @Quantity@ and omit @Items@ . In this configuration, CloudFront doesn't cache based on the values in the request headers. Regardless of which option you choose, CloudFront forwards headers to your origin based on whether the origin is an S3 bucket or a custom origin. See the following documentation:     * __S3 bucket__ : See <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/RequestAndResponseBehaviorS3Origin.html#request-s3-removed-headers HTTP Request Headers That CloudFront Removes or Updates>      * __Custom origin__ : See <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/RequestAndResponseBehaviorCustomOrigin.html#request-custom-headers-behavior HTTP Request Headers and CloudFront Behavior> 
headers
    :: Int -- ^ 'hQuantity'
    -> Headers
headers pQuantity_
  = Headers'{_hItems = Nothing,
             _hQuantity = pQuantity_}

-- | A list that contains one @Name@ element for each header that you want CloudFront to use for caching in this cache behavior. If @Quantity@ is @0@ , omit @Items@ .
hItems :: Lens' Headers [Text]
hItems = lens _hItems (\ s a -> s{_hItems = a}) . _Default . _Coerce

-- | The number of different headers that you want CloudFront to base caching on for this cache behavior. You can configure each cache behavior in a web distribution to do one of the following:     * __Forward all headers to your origin__ : Specify @1@ for @Quantity@ and @*@ for @Name@ . /Important:/ CloudFront doesn't cache the objects that are associated with this cache behavior. Instead, CloudFront sends every request to the origin.      * __Forward a whitelist of headers you specify__ : Specify the number of headers that you want CloudFront to base caching on. Then specify the header names in @Name@ elements. CloudFront caches your objects based on the values in the specified headers.     * __Forward only the default headers__ : Specify @0@ for @Quantity@ and omit @Items@ . In this configuration, CloudFront doesn't cache based on the values in the request headers. Regardless of which option you choose, CloudFront forwards headers to your origin based on whether the origin is an S3 bucket or a custom origin. See the following documentation:     * __S3 bucket__ : See <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/RequestAndResponseBehaviorS3Origin.html#request-s3-removed-headers HTTP Request Headers That CloudFront Removes or Updates>      * __Custom origin__ : See <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/RequestAndResponseBehaviorCustomOrigin.html#request-custom-headers-behavior HTTP Request Headers and CloudFront Behavior> 
hQuantity :: Lens' Headers Int
hQuantity = lens _hQuantity (\ s a -> s{_hQuantity = a})

instance FromXML Headers where
        parseXML x
          = Headers' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "Name"))
                <*> (x .@ "Quantity")

instance Hashable Headers where

instance NFData Headers where

instance ToXML Headers where
        toXML Headers'{..}
          = mconcat
              ["Items" @= toXML (toXMLList "Name" <$> _hItems),
               "Quantity" @= _hQuantity]
