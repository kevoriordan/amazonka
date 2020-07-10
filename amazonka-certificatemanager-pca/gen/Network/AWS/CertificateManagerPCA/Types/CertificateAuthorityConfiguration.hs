{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityConfiguration where

import Network.AWS.CertificateManagerPCA.Types.ASN1Subject
import Network.AWS.CertificateManagerPCA.Types.KeyAlgorithm
import Network.AWS.CertificateManagerPCA.Types.SigningAlgorithm
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains configuration information for your private certificate authority (CA). This includes information about the class of public key algorithm and the key pair that your private CA creates when it issues a certificate. It also includes the signature algorithm that it uses when issuing certificates, and its X.500 distinguished name. You must specify this information when you call the 'CreateCertificateAuthority' action. 
--
--
--
-- /See:/ 'certificateAuthorityConfiguration' smart constructor.
data CertificateAuthorityConfiguration = CertificateAuthorityConfiguration'{_cacKeyAlgorithm
                                                                            ::
                                                                            !KeyAlgorithm,
                                                                            _cacSigningAlgorithm
                                                                            ::
                                                                            !SigningAlgorithm,
                                                                            _cacSubject
                                                                            ::
                                                                            !ASN1Subject}
                                           deriving (Eq, Read, Show, Data,
                                                     Typeable, Generic)

-- | Creates a value of 'CertificateAuthorityConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cacKeyAlgorithm' - Type of the public key algorithm and size, in bits, of the key pair that your CA creates when it issues a certificate. When you create a subordinate CA, you must use a key algorithm supported by the parent CA.
--
-- * 'cacSigningAlgorithm' - Name of the algorithm your private CA uses to sign certificate requests.
--
-- * 'cacSubject' - Structure that contains X.500 distinguished name information for your private CA.
certificateAuthorityConfiguration
    :: KeyAlgorithm -- ^ 'cacKeyAlgorithm'
    -> SigningAlgorithm -- ^ 'cacSigningAlgorithm'
    -> ASN1Subject -- ^ 'cacSubject'
    -> CertificateAuthorityConfiguration
certificateAuthorityConfiguration pKeyAlgorithm_
  pSigningAlgorithm_ pSubject_
  = CertificateAuthorityConfiguration'{_cacKeyAlgorithm
                                         = pKeyAlgorithm_,
                                       _cacSigningAlgorithm =
                                         pSigningAlgorithm_,
                                       _cacSubject = pSubject_}

-- | Type of the public key algorithm and size, in bits, of the key pair that your CA creates when it issues a certificate. When you create a subordinate CA, you must use a key algorithm supported by the parent CA.
cacKeyAlgorithm :: Lens' CertificateAuthorityConfiguration KeyAlgorithm
cacKeyAlgorithm = lens _cacKeyAlgorithm (\ s a -> s{_cacKeyAlgorithm = a})

-- | Name of the algorithm your private CA uses to sign certificate requests.
cacSigningAlgorithm :: Lens' CertificateAuthorityConfiguration SigningAlgorithm
cacSigningAlgorithm = lens _cacSigningAlgorithm (\ s a -> s{_cacSigningAlgorithm = a})

-- | Structure that contains X.500 distinguished name information for your private CA.
cacSubject :: Lens' CertificateAuthorityConfiguration ASN1Subject
cacSubject = lens _cacSubject (\ s a -> s{_cacSubject = a})

instance FromJSON CertificateAuthorityConfiguration
         where
        parseJSON
          = withObject "CertificateAuthorityConfiguration"
              (\ x ->
                 CertificateAuthorityConfiguration' <$>
                   (x .: "KeyAlgorithm") <*> (x .: "SigningAlgorithm")
                     <*> (x .: "Subject"))

instance Hashable CertificateAuthorityConfiguration
         where

instance NFData CertificateAuthorityConfiguration
         where

instance ToJSON CertificateAuthorityConfiguration
         where
        toJSON CertificateAuthorityConfiguration'{..}
          = object
              (catMaybes
                 [Just ("KeyAlgorithm" .= _cacKeyAlgorithm),
                  Just ("SigningAlgorithm" .= _cacSigningAlgorithm),
                  Just ("Subject" .= _cacSubject)])
