{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.KeyPairIds
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.KeyPairIds where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that lists the active CloudFront key pairs, if any, that are associated with @AwsAccountNumber@ . 
--
--
-- For more information, see <https://docs.aws.amazon.com/cloudfront/latest/APIReference/API_ActiveTrustedSigners.html ActiveTrustedSigners> .
--
--
-- /See:/ 'keyPairIds' smart constructor.
data KeyPairIds = KeyPairIds'{_kpiItems ::
                              !(Maybe [Text]),
                              _kpiQuantity :: !Int}
                    deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KeyPairIds' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kpiItems' - A complex type that lists the active CloudFront key pairs, if any, that are associated with @AwsAccountNumber@ . For more information, see <https://docs.aws.amazon.com/cloudfront/latest/APIReference/API_ActiveTrustedSigners.html ActiveTrustedSigners> .
--
-- * 'kpiQuantity' - The number of active CloudFront key pairs for @AwsAccountNumber@ . For more information, see <https://docs.aws.amazon.com/cloudfront/latest/APIReference/API_ActiveTrustedSigners.html ActiveTrustedSigners> .
keyPairIds
    :: Int -- ^ 'kpiQuantity'
    -> KeyPairIds
keyPairIds pQuantity_
  = KeyPairIds'{_kpiItems = Nothing,
                _kpiQuantity = pQuantity_}

-- | A complex type that lists the active CloudFront key pairs, if any, that are associated with @AwsAccountNumber@ . For more information, see <https://docs.aws.amazon.com/cloudfront/latest/APIReference/API_ActiveTrustedSigners.html ActiveTrustedSigners> .
kpiItems :: Lens' KeyPairIds [Text]
kpiItems = lens _kpiItems (\ s a -> s{_kpiItems = a}) . _Default . _Coerce

-- | The number of active CloudFront key pairs for @AwsAccountNumber@ . For more information, see <https://docs.aws.amazon.com/cloudfront/latest/APIReference/API_ActiveTrustedSigners.html ActiveTrustedSigners> .
kpiQuantity :: Lens' KeyPairIds Int
kpiQuantity = lens _kpiQuantity (\ s a -> s{_kpiQuantity = a})

instance FromXML KeyPairIds where
        parseXML x
          = KeyPairIds' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "KeyPairId"))
                <*> (x .@ "Quantity")

instance Hashable KeyPairIds where

instance NFData KeyPairIds where
