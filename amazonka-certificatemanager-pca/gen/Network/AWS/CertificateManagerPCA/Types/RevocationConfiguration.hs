{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.RevocationConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManagerPCA.Types.RevocationConfiguration where

import Network.AWS.CertificateManagerPCA.Types.CrlConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Certificate revocation information used by the 'CreateCertificateAuthority' and 'UpdateCertificateAuthority' actions. Your private certificate authority (CA) can create and maintain a certificate revocation list (CRL). A CRL contains information about certificates revoked by your CA. For more information, see 'RevokeCertificate' .
--
--
--
-- /See:/ 'revocationConfiguration' smart constructor.
newtype RevocationConfiguration = RevocationConfiguration'{_rcCrlConfiguration
                                                           ::
                                                           Maybe
                                                             CrlConfiguration}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'RevocationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcCrlConfiguration' - Configuration of the certificate revocation list (CRL), if any, maintained by your private CA.
revocationConfiguration
    :: RevocationConfiguration
revocationConfiguration
  = RevocationConfiguration'{_rcCrlConfiguration =
                               Nothing}

-- | Configuration of the certificate revocation list (CRL), if any, maintained by your private CA.
rcCrlConfiguration :: Lens' RevocationConfiguration (Maybe CrlConfiguration)
rcCrlConfiguration = lens _rcCrlConfiguration (\ s a -> s{_rcCrlConfiguration = a})

instance FromJSON RevocationConfiguration where
        parseJSON
          = withObject "RevocationConfiguration"
              (\ x ->
                 RevocationConfiguration' <$>
                   (x .:? "CrlConfiguration"))

instance Hashable RevocationConfiguration where

instance NFData RevocationConfiguration where

instance ToJSON RevocationConfiguration where
        toJSON RevocationConfiguration'{..}
          = object
              (catMaybes
                 [("CrlConfiguration" .=) <$> _rcCrlConfiguration])
