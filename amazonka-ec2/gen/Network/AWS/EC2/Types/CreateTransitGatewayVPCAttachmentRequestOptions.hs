{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CreateTransitGatewayVPCAttachmentRequestOptions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.CreateTransitGatewayVPCAttachmentRequestOptions where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DNSSupportValue
import Network.AWS.EC2.Types.IPv6SupportValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the options for a VPC attachment.
--
--
--
-- /See:/ 'createTransitGatewayVPCAttachmentRequestOptions' smart constructor.
data CreateTransitGatewayVPCAttachmentRequestOptions = CreateTransitGatewayVPCAttachmentRequestOptions'{_ctgvaroIPv6Support
                                                                                                        ::
                                                                                                        !(Maybe
                                                                                                            IPv6SupportValue),
                                                                                                        _ctgvaroDNSSupport
                                                                                                        ::
                                                                                                        !(Maybe
                                                                                                            DNSSupportValue)}
                                                         deriving (Eq, Read,
                                                                   Show, Data,
                                                                   Typeable,
                                                                   Generic)

-- | Creates a value of 'CreateTransitGatewayVPCAttachmentRequestOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctgvaroIPv6Support' - Enable or disable IPv6 support. The default is @enable@ .
--
-- * 'ctgvaroDNSSupport' - Enable or disable DNS support. The default is @enable@ .
createTransitGatewayVPCAttachmentRequestOptions
    :: CreateTransitGatewayVPCAttachmentRequestOptions
createTransitGatewayVPCAttachmentRequestOptions
  = CreateTransitGatewayVPCAttachmentRequestOptions'{_ctgvaroIPv6Support
                                                       = Nothing,
                                                     _ctgvaroDNSSupport =
                                                       Nothing}

-- | Enable or disable IPv6 support. The default is @enable@ .
ctgvaroIPv6Support :: Lens' CreateTransitGatewayVPCAttachmentRequestOptions (Maybe IPv6SupportValue)
ctgvaroIPv6Support = lens _ctgvaroIPv6Support (\ s a -> s{_ctgvaroIPv6Support = a})

-- | Enable or disable DNS support. The default is @enable@ .
ctgvaroDNSSupport :: Lens' CreateTransitGatewayVPCAttachmentRequestOptions (Maybe DNSSupportValue)
ctgvaroDNSSupport = lens _ctgvaroDNSSupport (\ s a -> s{_ctgvaroDNSSupport = a})

instance Hashable
           CreateTransitGatewayVPCAttachmentRequestOptions
         where

instance NFData
           CreateTransitGatewayVPCAttachmentRequestOptions
         where

instance ToQuery
           CreateTransitGatewayVPCAttachmentRequestOptions
         where
        toQuery
          CreateTransitGatewayVPCAttachmentRequestOptions'{..}
          = mconcat
              ["Ipv6Support" =: _ctgvaroIPv6Support,
               "DnsSupport" =: _ctgvaroDNSSupport]
