{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateRenewalSummary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateRenewalSummary where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateDomainValidationOption
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateRenewalStatus
import Network.AWS.Prelude

-- | Contains information about the status of Lightsail's managed renewal for the certificate.
--
--
--
-- /See:/ 'loadBalancerTLSCertificateRenewalSummary' smart constructor.
data LoadBalancerTLSCertificateRenewalSummary = LoadBalancerTLSCertificateRenewalSummary'{_lbtcrsRenewalStatus
                                                                                          ::
                                                                                          !(Maybe
                                                                                              LoadBalancerTLSCertificateRenewalStatus),
                                                                                          _lbtcrsDomainValidationOptions
                                                                                          ::
                                                                                          !(Maybe
                                                                                              [LoadBalancerTLSCertificateDomainValidationOption])}
                                                  deriving (Eq, Read, Show,
                                                            Data, Typeable,
                                                            Generic)

-- | Creates a value of 'LoadBalancerTLSCertificateRenewalSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbtcrsRenewalStatus' - The status of Lightsail's managed renewal of the certificate. Valid values are listed below.
--
-- * 'lbtcrsDomainValidationOptions' - Contains information about the validation of each domain name in the certificate, as it pertains to Lightsail's managed renewal. This is different from the initial validation that occurs as a result of the RequestCertificate request.
loadBalancerTLSCertificateRenewalSummary
    :: LoadBalancerTLSCertificateRenewalSummary
loadBalancerTLSCertificateRenewalSummary
  = LoadBalancerTLSCertificateRenewalSummary'{_lbtcrsRenewalStatus
                                                = Nothing,
                                              _lbtcrsDomainValidationOptions =
                                                Nothing}

-- | The status of Lightsail's managed renewal of the certificate. Valid values are listed below.
lbtcrsRenewalStatus :: Lens' LoadBalancerTLSCertificateRenewalSummary (Maybe LoadBalancerTLSCertificateRenewalStatus)
lbtcrsRenewalStatus = lens _lbtcrsRenewalStatus (\ s a -> s{_lbtcrsRenewalStatus = a})

-- | Contains information about the validation of each domain name in the certificate, as it pertains to Lightsail's managed renewal. This is different from the initial validation that occurs as a result of the RequestCertificate request.
lbtcrsDomainValidationOptions :: Lens' LoadBalancerTLSCertificateRenewalSummary [LoadBalancerTLSCertificateDomainValidationOption]
lbtcrsDomainValidationOptions = lens _lbtcrsDomainValidationOptions (\ s a -> s{_lbtcrsDomainValidationOptions = a}) . _Default . _Coerce

instance FromJSON
           LoadBalancerTLSCertificateRenewalSummary
         where
        parseJSON
          = withObject
              "LoadBalancerTLSCertificateRenewalSummary"
              (\ x ->
                 LoadBalancerTLSCertificateRenewalSummary' <$>
                   (x .:? "renewalStatus") <*>
                     (x .:? "domainValidationOptions" .!= mempty))

instance Hashable
           LoadBalancerTLSCertificateRenewalSummary
         where

instance NFData
           LoadBalancerTLSCertificateRenewalSummary
         where
