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
-- Module      : Network.AWS.EC2.DeleteEgressOnlyInternetGateway
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an egress-only internet gateway.
--
--
module Network.AWS.EC2.DeleteEgressOnlyInternetGateway
    (
    -- * Creating a Request
      deleteEgressOnlyInternetGateway
    , DeleteEgressOnlyInternetGateway
    -- * Request Lenses
    , dltegrssonlyintrntgtwyDryRun
    , dltegrssonlyintrntgtwyEgressOnlyInternetGatewayId

    -- * Destructuring the Response
    , deleteEgressOnlyInternetGatewayResponse
    , DeleteEgressOnlyInternetGatewayResponse
    -- * Response Lenses
    , dltegrssonlyintrntgtwyrsReturnCode
    , dltegrssonlyintrntgtwyrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteEgressOnlyInternetGateway' smart constructor.
data DeleteEgressOnlyInternetGateway = DeleteEgressOnlyInternetGateway'{_dltegrssonlyintrntgtwyDryRun
                                                                        ::
                                                                        !(Maybe
                                                                            Bool),
                                                                        _dltegrssonlyintrntgtwyEgressOnlyInternetGatewayId
                                                                        ::
                                                                        !Text}
                                         deriving (Eq, Read, Show, Data,
                                                   Typeable, Generic)

-- | Creates a value of 'DeleteEgressOnlyInternetGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dltegrssonlyintrntgtwyDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dltegrssonlyintrntgtwyEgressOnlyInternetGatewayId' - The ID of the egress-only internet gateway.
deleteEgressOnlyInternetGateway
    :: Text -- ^ 'dltegrssonlyintrntgtwyEgressOnlyInternetGatewayId'
    -> DeleteEgressOnlyInternetGateway
deleteEgressOnlyInternetGateway
  pEgressOnlyInternetGatewayId_
  = DeleteEgressOnlyInternetGateway'{_dltegrssonlyintrntgtwyDryRun
                                       = Nothing,
                                     _dltegrssonlyintrntgtwyEgressOnlyInternetGatewayId
                                       = pEgressOnlyInternetGatewayId_}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dltegrssonlyintrntgtwyDryRun :: Lens' DeleteEgressOnlyInternetGateway (Maybe Bool)
dltegrssonlyintrntgtwyDryRun = lens _dltegrssonlyintrntgtwyDryRun (\ s a -> s{_dltegrssonlyintrntgtwyDryRun = a})

-- | The ID of the egress-only internet gateway.
dltegrssonlyintrntgtwyEgressOnlyInternetGatewayId :: Lens' DeleteEgressOnlyInternetGateway Text
dltegrssonlyintrntgtwyEgressOnlyInternetGatewayId = lens _dltegrssonlyintrntgtwyEgressOnlyInternetGatewayId (\ s a -> s{_dltegrssonlyintrntgtwyEgressOnlyInternetGatewayId = a})

instance AWSRequest DeleteEgressOnlyInternetGateway
         where
        type Rs DeleteEgressOnlyInternetGateway =
             DeleteEgressOnlyInternetGatewayResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DeleteEgressOnlyInternetGatewayResponse' <$>
                   (x .@? "returnCode") <*> (pure (fromEnum s)))

instance Hashable DeleteEgressOnlyInternetGateway
         where

instance NFData DeleteEgressOnlyInternetGateway where

instance ToHeaders DeleteEgressOnlyInternetGateway
         where
        toHeaders = const mempty

instance ToPath DeleteEgressOnlyInternetGateway where
        toPath = const "/"

instance ToQuery DeleteEgressOnlyInternetGateway
         where
        toQuery DeleteEgressOnlyInternetGateway'{..}
          = mconcat
              ["Action" =:
                 ("DeleteEgressOnlyInternetGateway" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dltegrssonlyintrntgtwyDryRun,
               "EgressOnlyInternetGatewayId" =:
                 _dltegrssonlyintrntgtwyEgressOnlyInternetGatewayId]

-- | /See:/ 'deleteEgressOnlyInternetGatewayResponse' smart constructor.
data DeleteEgressOnlyInternetGatewayResponse = DeleteEgressOnlyInternetGatewayResponse'{_dltegrssonlyintrntgtwyrsReturnCode
                                                                                        ::
                                                                                        !(Maybe
                                                                                            Bool),
                                                                                        _dltegrssonlyintrntgtwyrsResponseStatus
                                                                                        ::
                                                                                        !Int}
                                                 deriving (Eq, Read, Show, Data,
                                                           Typeable, Generic)

-- | Creates a value of 'DeleteEgressOnlyInternetGatewayResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dltegrssonlyintrntgtwyrsReturnCode' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- * 'dltegrssonlyintrntgtwyrsResponseStatus' - -- | The response status code.
deleteEgressOnlyInternetGatewayResponse
    :: Int -- ^ 'dltegrssonlyintrntgtwyrsResponseStatus'
    -> DeleteEgressOnlyInternetGatewayResponse
deleteEgressOnlyInternetGatewayResponse
  pResponseStatus_
  = DeleteEgressOnlyInternetGatewayResponse'{_dltegrssonlyintrntgtwyrsReturnCode
                                               = Nothing,
                                             _dltegrssonlyintrntgtwyrsResponseStatus
                                               = pResponseStatus_}

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
dltegrssonlyintrntgtwyrsReturnCode :: Lens' DeleteEgressOnlyInternetGatewayResponse (Maybe Bool)
dltegrssonlyintrntgtwyrsReturnCode = lens _dltegrssonlyintrntgtwyrsReturnCode (\ s a -> s{_dltegrssonlyintrntgtwyrsReturnCode = a})

-- | -- | The response status code.
dltegrssonlyintrntgtwyrsResponseStatus :: Lens' DeleteEgressOnlyInternetGatewayResponse Int
dltegrssonlyintrntgtwyrsResponseStatus = lens _dltegrssonlyintrntgtwyrsResponseStatus (\ s a -> s{_dltegrssonlyintrntgtwyrsResponseStatus = a})

instance NFData
           DeleteEgressOnlyInternetGatewayResponse
         where
