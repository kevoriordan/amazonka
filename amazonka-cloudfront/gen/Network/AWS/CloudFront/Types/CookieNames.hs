{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CookieNames
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.CookieNames where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that specifies whether you want CloudFront to forward cookies to the origin and, if so, which ones. For more information about forwarding cookies to the origin, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/header-caching.html Caching Content Based on Request Headers> in the /Amazon CloudFront Developer Guide/ .
--
--
--
-- /See:/ 'cookieNames' smart constructor.
data CookieNames = CookieNames'{_cnItems ::
                                !(Maybe [Text]),
                                _cnQuantity :: !Int}
                     deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CookieNames' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnItems' - A complex type that contains one @Name@ element for each cookie that you want CloudFront to forward to the origin for this cache behavior. It must contain the same number of items that is specified in the @Quantity@ field. When you set @Forward = whitelist@ (in the @CookiePreferences@ object), this field must contain at least one item.
--
-- * 'cnQuantity' - The number of different cookies that you want CloudFront to forward to the origin for this cache behavior. The value must equal the number of items that are in the @Items@ field. When you set @Forward = whitelist@ (in the @CookiePreferences@ object), this value must be @1@ or higher.
cookieNames
    :: Int -- ^ 'cnQuantity'
    -> CookieNames
cookieNames pQuantity_
  = CookieNames'{_cnItems = Nothing,
                 _cnQuantity = pQuantity_}

-- | A complex type that contains one @Name@ element for each cookie that you want CloudFront to forward to the origin for this cache behavior. It must contain the same number of items that is specified in the @Quantity@ field. When you set @Forward = whitelist@ (in the @CookiePreferences@ object), this field must contain at least one item.
cnItems :: Lens' CookieNames [Text]
cnItems = lens _cnItems (\ s a -> s{_cnItems = a}) . _Default . _Coerce

-- | The number of different cookies that you want CloudFront to forward to the origin for this cache behavior. The value must equal the number of items that are in the @Items@ field. When you set @Forward = whitelist@ (in the @CookiePreferences@ object), this value must be @1@ or higher.
cnQuantity :: Lens' CookieNames Int
cnQuantity = lens _cnQuantity (\ s a -> s{_cnQuantity = a})

instance FromXML CookieNames where
        parseXML x
          = CookieNames' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "Name"))
                <*> (x .@ "Quantity")

instance Hashable CookieNames where

instance NFData CookieNames where

instance ToXML CookieNames where
        toXML CookieNames'{..}
          = mconcat
              ["Items" @= toXML (toXMLList "Name" <$> _cnItems),
               "Quantity" @= _cnQuantity]
