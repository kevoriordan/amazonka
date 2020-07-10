{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Origins
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.Origins where

import Network.AWS.CloudFront.Types.Origin
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that contains information about origins and origin groups for this distribution. 
--
--
--
-- /See:/ 'origins' smart constructor.
data Origins = Origins'{_oQuantity :: !Int,
                        _oItems :: !(List1 Origin)}
                 deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Origins' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oQuantity' - The number of origins or origin groups for this distribution.
--
-- * 'oItems' - A complex type that contains origins or origin groups for this distribution.
origins
    :: Int -- ^ 'oQuantity'
    -> NonEmpty Origin -- ^ 'oItems'
    -> Origins
origins pQuantity_ pItems_
  = Origins'{_oQuantity = pQuantity_,
             _oItems = _List1 # pItems_}

-- | The number of origins or origin groups for this distribution.
oQuantity :: Lens' Origins Int
oQuantity = lens _oQuantity (\ s a -> s{_oQuantity = a})

-- | A complex type that contains origins or origin groups for this distribution.
oItems :: Lens' Origins (NonEmpty Origin)
oItems = lens _oItems (\ s a -> s{_oItems = a}) . _List1

instance FromXML Origins where
        parseXML x
          = Origins' <$>
              (x .@ "Quantity") <*>
                (x .@? "Items" .!@ mempty >>= parseXMLList1 "Origin")

instance Hashable Origins where

instance NFData Origins where

instance ToXML Origins where
        toXML Origins'{..}
          = mconcat
              ["Quantity" @= _oQuantity,
               "Items" @= toXMLList "Origin" _oItems]
