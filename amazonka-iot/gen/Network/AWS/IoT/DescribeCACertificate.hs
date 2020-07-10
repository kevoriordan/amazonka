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
-- Module      : Network.AWS.IoT.DescribeCACertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a registered CA certificate.
--
--
module Network.AWS.IoT.DescribeCACertificate
    (
    -- * Creating a Request
      describeCACertificate
    , DescribeCACertificate
    -- * Request Lenses
    , dCertificateId

    -- * Destructuring the Response
    , describeCACertificateResponse
    , DescribeCACertificateResponse
    -- * Response Lenses
    , dscrbcacrtfctrsCertificateDescription
    , dscrbcacrtfctrsRegistrationConfig
    , dscrbcacrtfctrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the DescribeCACertificate operation.
--
--
--
-- /See:/ 'describeCACertificate' smart constructor.
newtype DescribeCACertificate = DescribeCACertificate'{_dCertificateId
                                                       :: Text}
                                  deriving (Eq, Read, Show, Data, Typeable,
                                            Generic)

-- | Creates a value of 'DescribeCACertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dCertificateId' - The CA certificate identifier.
describeCACertificate
    :: Text -- ^ 'dCertificateId'
    -> DescribeCACertificate
describeCACertificate pCertificateId_
  = DescribeCACertificate'{_dCertificateId =
                             pCertificateId_}

-- | The CA certificate identifier.
dCertificateId :: Lens' DescribeCACertificate Text
dCertificateId = lens _dCertificateId (\ s a -> s{_dCertificateId = a})

instance AWSRequest DescribeCACertificate where
        type Rs DescribeCACertificate =
             DescribeCACertificateResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 DescribeCACertificateResponse' <$>
                   (x .?> "certificateDescription") <*>
                     (x .?> "registrationConfig")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeCACertificate where

instance NFData DescribeCACertificate where

instance ToHeaders DescribeCACertificate where
        toHeaders = const mempty

instance ToPath DescribeCACertificate where
        toPath DescribeCACertificate'{..}
          = mconcat ["/cacertificate/", toBS _dCertificateId]

instance ToQuery DescribeCACertificate where
        toQuery = const mempty

-- | The output from the DescribeCACertificate operation.
--
--
--
-- /See:/ 'describeCACertificateResponse' smart constructor.
data DescribeCACertificateResponse = DescribeCACertificateResponse'{_dscrbcacrtfctrsCertificateDescription
                                                                    ::
                                                                    !(Maybe
                                                                        CACertificateDescription),
                                                                    _dscrbcacrtfctrsRegistrationConfig
                                                                    ::
                                                                    !(Maybe
                                                                        RegistrationConfig),
                                                                    _dscrbcacrtfctrsResponseStatus
                                                                    :: !Int}
                                       deriving (Eq, Read, Show, Data, Typeable,
                                                 Generic)

-- | Creates a value of 'DescribeCACertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscrbcacrtfctrsCertificateDescription' - The CA certificate description.
--
-- * 'dscrbcacrtfctrsRegistrationConfig' - Information about the registration configuration.
--
-- * 'dscrbcacrtfctrsResponseStatus' - -- | The response status code.
describeCACertificateResponse
    :: Int -- ^ 'dscrbcacrtfctrsResponseStatus'
    -> DescribeCACertificateResponse
describeCACertificateResponse pResponseStatus_
  = DescribeCACertificateResponse'{_dscrbcacrtfctrsCertificateDescription
                                     = Nothing,
                                   _dscrbcacrtfctrsRegistrationConfig = Nothing,
                                   _dscrbcacrtfctrsResponseStatus =
                                     pResponseStatus_}

-- | The CA certificate description.
dscrbcacrtfctrsCertificateDescription :: Lens' DescribeCACertificateResponse (Maybe CACertificateDescription)
dscrbcacrtfctrsCertificateDescription = lens _dscrbcacrtfctrsCertificateDescription (\ s a -> s{_dscrbcacrtfctrsCertificateDescription = a})

-- | Information about the registration configuration.
dscrbcacrtfctrsRegistrationConfig :: Lens' DescribeCACertificateResponse (Maybe RegistrationConfig)
dscrbcacrtfctrsRegistrationConfig = lens _dscrbcacrtfctrsRegistrationConfig (\ s a -> s{_dscrbcacrtfctrsRegistrationConfig = a})

-- | -- | The response status code.
dscrbcacrtfctrsResponseStatus :: Lens' DescribeCACertificateResponse Int
dscrbcacrtfctrsResponseStatus = lens _dscrbcacrtfctrsResponseStatus (\ s a -> s{_dscrbcacrtfctrsResponseStatus = a})

instance NFData DescribeCACertificateResponse where
