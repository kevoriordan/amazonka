{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.Matcher
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.Matcher where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information to use when checking for a successful response from a target.
--
--
--
-- /See:/ 'matcher' smart constructor.
newtype Matcher = Matcher'{_mHTTPCode :: Text}
                    deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Matcher' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mHTTPCode' - The HTTP codes. For Application Load Balancers, you can specify values between 200 and 499, and the default value is 200. You can specify multiple values (for example, "200,202") or a range of values (for example, "200-299"). For Network Load Balancers, this is 200–399.
matcher
    :: Text -- ^ 'mHTTPCode'
    -> Matcher
matcher pHTTPCode_
  = Matcher'{_mHTTPCode = pHTTPCode_}

-- | The HTTP codes. For Application Load Balancers, you can specify values between 200 and 499, and the default value is 200. You can specify multiple values (for example, "200,202") or a range of values (for example, "200-299"). For Network Load Balancers, this is 200–399.
mHTTPCode :: Lens' Matcher Text
mHTTPCode = lens _mHTTPCode (\ s a -> s{_mHTTPCode = a})

instance FromXML Matcher where
        parseXML x = Matcher' <$> (x .@ "HttpCode")

instance Hashable Matcher where

instance NFData Matcher where

instance ToQuery Matcher where
        toQuery Matcher'{..}
          = mconcat ["HttpCode" =: _mHTTPCode]
