{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CookiePreference
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.CookiePreference where

import Network.AWS.CloudFront.Types.CookieNames
import Network.AWS.CloudFront.Types.ItemSelection
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that specifies whether you want CloudFront to forward cookies to the origin and, if so, which ones. For more information about forwarding cookies to the origin, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Cookies.html Caching Content Based on Cookies> in the /Amazon CloudFront Developer Guide/ .
--
--
--
-- /See:/ 'cookiePreference' smart constructor.
data CookiePreference = CookiePreference'{_cpWhitelistedNames
                                          :: !(Maybe CookieNames),
                                          _cpForward :: !ItemSelection}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CookiePreference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpWhitelistedNames' - Required if you specify @whitelist@ for the value of @Forward@ . A complex type that specifies how many different cookies you want CloudFront to forward to the origin for this cache behavior and, if you want to forward selected cookies, the names of those cookies. If you specify @all@ or @none@ for the value of @Forward@ , omit @WhitelistedNames@ . If you change the value of @Forward@ from @whitelist@ to @all@ or @none@ and you don't delete the @WhitelistedNames@ element and its child elements, CloudFront deletes them automatically. For the current limit on the number of cookie names that you can whitelist for each cache behavior, see <https://docs.aws.amazon.com/general/latest/gr/xrefaws_service_limits.html#limits_cloudfront CloudFront Limits> in the /AWS General Reference/ .
--
-- * 'cpForward' - Specifies which cookies to forward to the origin for this cache behavior: all, none, or the list of cookies specified in the @WhitelistedNames@ complex type. Amazon S3 doesn't process cookies. When the cache behavior is forwarding requests to an Amazon S3 origin, specify none for the @Forward@ element.
cookiePreference
    :: ItemSelection -- ^ 'cpForward'
    -> CookiePreference
cookiePreference pForward_
  = CookiePreference'{_cpWhitelistedNames = Nothing,
                      _cpForward = pForward_}

-- | Required if you specify @whitelist@ for the value of @Forward@ . A complex type that specifies how many different cookies you want CloudFront to forward to the origin for this cache behavior and, if you want to forward selected cookies, the names of those cookies. If you specify @all@ or @none@ for the value of @Forward@ , omit @WhitelistedNames@ . If you change the value of @Forward@ from @whitelist@ to @all@ or @none@ and you don't delete the @WhitelistedNames@ element and its child elements, CloudFront deletes them automatically. For the current limit on the number of cookie names that you can whitelist for each cache behavior, see <https://docs.aws.amazon.com/general/latest/gr/xrefaws_service_limits.html#limits_cloudfront CloudFront Limits> in the /AWS General Reference/ .
cpWhitelistedNames :: Lens' CookiePreference (Maybe CookieNames)
cpWhitelistedNames = lens _cpWhitelistedNames (\ s a -> s{_cpWhitelistedNames = a})

-- | Specifies which cookies to forward to the origin for this cache behavior: all, none, or the list of cookies specified in the @WhitelistedNames@ complex type. Amazon S3 doesn't process cookies. When the cache behavior is forwarding requests to an Amazon S3 origin, specify none for the @Forward@ element.
cpForward :: Lens' CookiePreference ItemSelection
cpForward = lens _cpForward (\ s a -> s{_cpForward = a})

instance FromXML CookiePreference where
        parseXML x
          = CookiePreference' <$>
              (x .@? "WhitelistedNames") <*> (x .@ "Forward")

instance Hashable CookiePreference where

instance NFData CookiePreference where

instance ToXML CookiePreference where
        toXML CookiePreference'{..}
          = mconcat
              ["WhitelistedNames" @= _cpWhitelistedNames,
               "Forward" @= _cpForward]
