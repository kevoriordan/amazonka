{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisassociateTransitGatewayMulticastDomain
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified subnets from the transit gateway multicast domain. 
--
--
module Network.AWS.EC2.DisassociateTransitGatewayMulticastDomain
    (
    -- * Creating a Request
      disassociateTransitGatewayMulticastDomain
    , DisassociateTransitGatewayMulticastDomain
    -- * Request Lenses
    , dssscttrnstgtwymltcstdmnSubnetIds
    , dssscttrnstgtwymltcstdmnTransitGatewayMulticastDomainId
    , dssscttrnstgtwymltcstdmnTransitGatewayAttachmentId
    , dssscttrnstgtwymltcstdmnDryRun

    -- * Destructuring the Response
    , disassociateTransitGatewayMulticastDomainResponse
    , DisassociateTransitGatewayMulticastDomainResponse
    -- * Response Lenses
    , dtgmdrsAssociations
    , dtgmdrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateTransitGatewayMulticastDomain' smart constructor.
data DisassociateTransitGatewayMulticastDomain = DisassociateTransitGatewayMulticastDomain'{_dssscttrnstgtwymltcstdmnSubnetIds
                                                                                            ::
                                                                                            !(Maybe
                                                                                                [Text]),
                                                                                            _dssscttrnstgtwymltcstdmnTransitGatewayMulticastDomainId
                                                                                            ::
                                                                                            !(Maybe
                                                                                                Text),
                                                                                            _dssscttrnstgtwymltcstdmnTransitGatewayAttachmentId
                                                                                            ::
                                                                                            !(Maybe
                                                                                                Text),
                                                                                            _dssscttrnstgtwymltcstdmnDryRun
                                                                                            ::
                                                                                            !(Maybe
                                                                                                Bool)}
                                                   deriving (Eq, Read, Show,
                                                             Data, Typeable,
                                                             Generic)

-- | Creates a value of 'DisassociateTransitGatewayMulticastDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssscttrnstgtwymltcstdmnSubnetIds' - The IDs of the subnets;
--
-- * 'dssscttrnstgtwymltcstdmnTransitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
--
-- * 'dssscttrnstgtwymltcstdmnTransitGatewayAttachmentId' - The ID of the attachment.
--
-- * 'dssscttrnstgtwymltcstdmnDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
disassociateTransitGatewayMulticastDomain
    :: DisassociateTransitGatewayMulticastDomain
disassociateTransitGatewayMulticastDomain
  = DisassociateTransitGatewayMulticastDomain'{_dssscttrnstgtwymltcstdmnSubnetIds
                                                 = Nothing,
                                               _dssscttrnstgtwymltcstdmnTransitGatewayMulticastDomainId
                                                 = Nothing,
                                               _dssscttrnstgtwymltcstdmnTransitGatewayAttachmentId
                                                 = Nothing,
                                               _dssscttrnstgtwymltcstdmnDryRun =
                                                 Nothing}

-- | The IDs of the subnets;
dssscttrnstgtwymltcstdmnSubnetIds :: Lens' DisassociateTransitGatewayMulticastDomain [Text]
dssscttrnstgtwymltcstdmnSubnetIds = lens _dssscttrnstgtwymltcstdmnSubnetIds (\ s a -> s{_dssscttrnstgtwymltcstdmnSubnetIds = a}) . _Default . _Coerce

-- | The ID of the transit gateway multicast domain.
dssscttrnstgtwymltcstdmnTransitGatewayMulticastDomainId :: Lens' DisassociateTransitGatewayMulticastDomain (Maybe Text)
dssscttrnstgtwymltcstdmnTransitGatewayMulticastDomainId = lens _dssscttrnstgtwymltcstdmnTransitGatewayMulticastDomainId (\ s a -> s{_dssscttrnstgtwymltcstdmnTransitGatewayMulticastDomainId = a})

-- | The ID of the attachment.
dssscttrnstgtwymltcstdmnTransitGatewayAttachmentId :: Lens' DisassociateTransitGatewayMulticastDomain (Maybe Text)
dssscttrnstgtwymltcstdmnTransitGatewayAttachmentId = lens _dssscttrnstgtwymltcstdmnTransitGatewayAttachmentId (\ s a -> s{_dssscttrnstgtwymltcstdmnTransitGatewayAttachmentId = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dssscttrnstgtwymltcstdmnDryRun :: Lens' DisassociateTransitGatewayMulticastDomain (Maybe Bool)
dssscttrnstgtwymltcstdmnDryRun = lens _dssscttrnstgtwymltcstdmnDryRun (\ s a -> s{_dssscttrnstgtwymltcstdmnDryRun = a})

instance AWSRequest
           DisassociateTransitGatewayMulticastDomain
         where
        type Rs DisassociateTransitGatewayMulticastDomain =
             DisassociateTransitGatewayMulticastDomainResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DisassociateTransitGatewayMulticastDomainResponse'
                   <$> (x .@? "associations") <*> (pure (fromEnum s)))

instance Hashable
           DisassociateTransitGatewayMulticastDomain
         where

instance NFData
           DisassociateTransitGatewayMulticastDomain
         where

instance ToHeaders
           DisassociateTransitGatewayMulticastDomain
         where
        toHeaders = const mempty

instance ToPath
           DisassociateTransitGatewayMulticastDomain
         where
        toPath = const "/"

instance ToQuery
           DisassociateTransitGatewayMulticastDomain
         where
        toQuery
          DisassociateTransitGatewayMulticastDomain'{..}
          = mconcat
              ["Action" =:
                 ("DisassociateTransitGatewayMulticastDomain" ::
                    ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery
                 (toQueryList "SubnetIds" <$>
                    _dssscttrnstgtwymltcstdmnSubnetIds),
               "TransitGatewayMulticastDomainId" =:
                 _dssscttrnstgtwymltcstdmnTransitGatewayMulticastDomainId,
               "TransitGatewayAttachmentId" =:
                 _dssscttrnstgtwymltcstdmnTransitGatewayAttachmentId,
               "DryRun" =: _dssscttrnstgtwymltcstdmnDryRun]

-- | /See:/ 'disassociateTransitGatewayMulticastDomainResponse' smart constructor.
data DisassociateTransitGatewayMulticastDomainResponse = DisassociateTransitGatewayMulticastDomainResponse'{_dtgmdrsAssociations
                                                                                                            ::
                                                                                                            !(Maybe
                                                                                                                TransitGatewayMulticastDomainAssociations),
                                                                                                            _dtgmdrsResponseStatus
                                                                                                            ::
                                                                                                            !Int}
                                                           deriving (Eq, Read,
                                                                     Show, Data,
                                                                     Typeable,
                                                                     Generic)

-- | Creates a value of 'DisassociateTransitGatewayMulticastDomainResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgmdrsAssociations' - Information about the association.
--
-- * 'dtgmdrsResponseStatus' - -- | The response status code.
disassociateTransitGatewayMulticastDomainResponse
    :: Int -- ^ 'dtgmdrsResponseStatus'
    -> DisassociateTransitGatewayMulticastDomainResponse
disassociateTransitGatewayMulticastDomainResponse
  pResponseStatus_
  = DisassociateTransitGatewayMulticastDomainResponse'{_dtgmdrsAssociations
                                                         = Nothing,
                                                       _dtgmdrsResponseStatus =
                                                         pResponseStatus_}

-- | Information about the association.
dtgmdrsAssociations :: Lens' DisassociateTransitGatewayMulticastDomainResponse (Maybe TransitGatewayMulticastDomainAssociations)
dtgmdrsAssociations = lens _dtgmdrsAssociations (\ s a -> s{_dtgmdrsAssociations = a})

-- | -- | The response status code.
dtgmdrsResponseStatus :: Lens' DisassociateTransitGatewayMulticastDomainResponse Int
dtgmdrsResponseStatus = lens _dtgmdrsResponseStatus (\ s a -> s{_dtgmdrsResponseStatus = a})

instance NFData
           DisassociateTransitGatewayMulticastDomainResponse
         where
