{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.Validity
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManagerPCA.Types.Validity where

import Network.AWS.CertificateManagerPCA.Types.ValidityPeriodType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Length of time for which the certificate issued by your private certificate authority (CA), or by the private CA itself, is valid in days, months, or years. You can issue a certificate by calling the 'IssueCertificate' action.
--
--
--
-- /See:/ 'validity' smart constructor.
data Validity = Validity'{_vValue :: !Nat,
                          _vType :: !ValidityPeriodType}
                  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Validity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vValue' - Time period.
--
-- * 'vType' - Specifies whether the @Value@ parameter represents days, months, or years.
validity
    :: Natural -- ^ 'vValue'
    -> ValidityPeriodType -- ^ 'vType'
    -> Validity
validity pValue_ pType_
  = Validity'{_vValue = _Nat # pValue_,
              _vType = pType_}

-- | Time period.
vValue :: Lens' Validity Natural
vValue = lens _vValue (\ s a -> s{_vValue = a}) . _Nat

-- | Specifies whether the @Value@ parameter represents days, months, or years.
vType :: Lens' Validity ValidityPeriodType
vType = lens _vType (\ s a -> s{_vType = a})

instance Hashable Validity where

instance NFData Validity where

instance ToJSON Validity where
        toJSON Validity'{..}
          = object
              (catMaybes
                 [Just ("Value" .= _vValue), Just ("Type" .= _vType)])
