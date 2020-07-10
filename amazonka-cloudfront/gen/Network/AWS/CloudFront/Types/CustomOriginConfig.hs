{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CustomOriginConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.CustomOriginConfig where

import Network.AWS.CloudFront.Types.OriginProtocolPolicy
import Network.AWS.CloudFront.Types.OriginSSLProtocols
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A custom origin or an Amazon S3 bucket configured as a website endpoint.
--
--
--
-- /See:/ 'customOriginConfig' smart constructor.
data CustomOriginConfig = CustomOriginConfig'{_cocOriginKeepaliveTimeout
                                              :: !(Maybe Int),
                                              _cocOriginReadTimeout ::
                                              !(Maybe Int),
                                              _cocOriginSSLProtocols ::
                                              !(Maybe OriginSSLProtocols),
                                              _cocHTTPPort :: !Int,
                                              _cocHTTPSPort :: !Int,
                                              _cocOriginProtocolPolicy ::
                                              !OriginProtocolPolicy}
                            deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CustomOriginConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cocOriginKeepaliveTimeout' - You can create a custom keep-alive timeout. All timeout units are in seconds. The default keep-alive timeout is 5 seconds, but you can configure custom timeout lengths using the CloudFront API. The minimum timeout length is 1 second; the maximum is 60 seconds. If you need to increase the maximum time limit, contact the <https://console.aws.amazon.com/support/home#/ AWS Support Center> .
--
-- * 'cocOriginReadTimeout' - You can create a custom origin read timeout. All timeout units are in seconds. The default origin read timeout is 30 seconds, but you can configure custom timeout lengths using the CloudFront API. The minimum timeout length is 4 seconds; the maximum is 60 seconds. If you need to increase the maximum time limit, contact the <https://console.aws.amazon.com/support/home#/ AWS Support Center> .
--
-- * 'cocOriginSSLProtocols' - The SSL/TLS protocols that you want CloudFront to use when communicating with your origin over HTTPS.
--
-- * 'cocHTTPPort' - The HTTP port the custom origin listens on.
--
-- * 'cocHTTPSPort' - The HTTPS port the custom origin listens on.
--
-- * 'cocOriginProtocolPolicy' - The origin protocol policy to apply to your origin.
customOriginConfig
    :: Int -- ^ 'cocHTTPPort'
    -> Int -- ^ 'cocHTTPSPort'
    -> OriginProtocolPolicy -- ^ 'cocOriginProtocolPolicy'
    -> CustomOriginConfig
customOriginConfig pHTTPPort_ pHTTPSPort_
  pOriginProtocolPolicy_
  = CustomOriginConfig'{_cocOriginKeepaliveTimeout =
                          Nothing,
                        _cocOriginReadTimeout = Nothing,
                        _cocOriginSSLProtocols = Nothing,
                        _cocHTTPPort = pHTTPPort_,
                        _cocHTTPSPort = pHTTPSPort_,
                        _cocOriginProtocolPolicy = pOriginProtocolPolicy_}

-- | You can create a custom keep-alive timeout. All timeout units are in seconds. The default keep-alive timeout is 5 seconds, but you can configure custom timeout lengths using the CloudFront API. The minimum timeout length is 1 second; the maximum is 60 seconds. If you need to increase the maximum time limit, contact the <https://console.aws.amazon.com/support/home#/ AWS Support Center> .
cocOriginKeepaliveTimeout :: Lens' CustomOriginConfig (Maybe Int)
cocOriginKeepaliveTimeout = lens _cocOriginKeepaliveTimeout (\ s a -> s{_cocOriginKeepaliveTimeout = a})

-- | You can create a custom origin read timeout. All timeout units are in seconds. The default origin read timeout is 30 seconds, but you can configure custom timeout lengths using the CloudFront API. The minimum timeout length is 4 seconds; the maximum is 60 seconds. If you need to increase the maximum time limit, contact the <https://console.aws.amazon.com/support/home#/ AWS Support Center> .
cocOriginReadTimeout :: Lens' CustomOriginConfig (Maybe Int)
cocOriginReadTimeout = lens _cocOriginReadTimeout (\ s a -> s{_cocOriginReadTimeout = a})

-- | The SSL/TLS protocols that you want CloudFront to use when communicating with your origin over HTTPS.
cocOriginSSLProtocols :: Lens' CustomOriginConfig (Maybe OriginSSLProtocols)
cocOriginSSLProtocols = lens _cocOriginSSLProtocols (\ s a -> s{_cocOriginSSLProtocols = a})

-- | The HTTP port the custom origin listens on.
cocHTTPPort :: Lens' CustomOriginConfig Int
cocHTTPPort = lens _cocHTTPPort (\ s a -> s{_cocHTTPPort = a})

-- | The HTTPS port the custom origin listens on.
cocHTTPSPort :: Lens' CustomOriginConfig Int
cocHTTPSPort = lens _cocHTTPSPort (\ s a -> s{_cocHTTPSPort = a})

-- | The origin protocol policy to apply to your origin.
cocOriginProtocolPolicy :: Lens' CustomOriginConfig OriginProtocolPolicy
cocOriginProtocolPolicy = lens _cocOriginProtocolPolicy (\ s a -> s{_cocOriginProtocolPolicy = a})

instance FromXML CustomOriginConfig where
        parseXML x
          = CustomOriginConfig' <$>
              (x .@? "OriginKeepaliveTimeout") <*>
                (x .@? "OriginReadTimeout")
                <*> (x .@? "OriginSslProtocols")
                <*> (x .@ "HTTPPort")
                <*> (x .@ "HTTPSPort")
                <*> (x .@ "OriginProtocolPolicy")

instance Hashable CustomOriginConfig where

instance NFData CustomOriginConfig where

instance ToXML CustomOriginConfig where
        toXML CustomOriginConfig'{..}
          = mconcat
              ["OriginKeepaliveTimeout" @=
                 _cocOriginKeepaliveTimeout,
               "OriginReadTimeout" @= _cocOriginReadTimeout,
               "OriginSslProtocols" @= _cocOriginSSLProtocols,
               "HTTPPort" @= _cocHTTPPort,
               "HTTPSPort" @= _cocHTTPSPort,
               "OriginProtocolPolicy" @= _cocOriginProtocolPolicy]
