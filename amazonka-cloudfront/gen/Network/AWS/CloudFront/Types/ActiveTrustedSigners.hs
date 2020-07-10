{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.ActiveTrustedSigners
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.ActiveTrustedSigners where

import Network.AWS.CloudFront.Types.Signer
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that lists the AWS accounts, if any, that you included in the @TrustedSigners@ complex type for this distribution. These are the accounts that you want to allow to create signed URLs for private content.
--
--
-- The @Signer@ complex type lists the AWS account number of the trusted signer or @self@ if the signer is the AWS account that created the distribution. The @Signer@ element also includes the IDs of any active CloudFront key pairs that are associated with the trusted signer's AWS account. If no @KeyPairId@ element appears for a @Signer@ , that signer can't create signed URLs. 
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ .
--
--
-- /See:/ 'activeTrustedSigners' smart constructor.
data ActiveTrustedSigners = ActiveTrustedSigners'{_atsItems
                                                  :: !(Maybe [Signer]),
                                                  _atsEnabled :: !Bool,
                                                  _atsQuantity :: !Int}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActiveTrustedSigners' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atsItems' - A complex type that contains one @Signer@ complex type for each trusted signer that is specified in the @TrustedSigners@ complex type.
--
-- * 'atsEnabled' - Enabled is @true@ if any of the AWS accounts listed in the @TrustedSigners@ complex type for this distribution have active CloudFront key pairs. If not, @Enabled@ is @false@ .
--
-- * 'atsQuantity' - The number of trusted signers specified in the @TrustedSigners@ complex type.
activeTrustedSigners
    :: Bool -- ^ 'atsEnabled'
    -> Int -- ^ 'atsQuantity'
    -> ActiveTrustedSigners
activeTrustedSigners pEnabled_ pQuantity_
  = ActiveTrustedSigners'{_atsItems = Nothing,
                          _atsEnabled = pEnabled_, _atsQuantity = pQuantity_}

-- | A complex type that contains one @Signer@ complex type for each trusted signer that is specified in the @TrustedSigners@ complex type.
atsItems :: Lens' ActiveTrustedSigners [Signer]
atsItems = lens _atsItems (\ s a -> s{_atsItems = a}) . _Default . _Coerce

-- | Enabled is @true@ if any of the AWS accounts listed in the @TrustedSigners@ complex type for this distribution have active CloudFront key pairs. If not, @Enabled@ is @false@ .
atsEnabled :: Lens' ActiveTrustedSigners Bool
atsEnabled = lens _atsEnabled (\ s a -> s{_atsEnabled = a})

-- | The number of trusted signers specified in the @TrustedSigners@ complex type.
atsQuantity :: Lens' ActiveTrustedSigners Int
atsQuantity = lens _atsQuantity (\ s a -> s{_atsQuantity = a})

instance FromXML ActiveTrustedSigners where
        parseXML x
          = ActiveTrustedSigners' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "Signer"))
                <*> (x .@ "Enabled")
                <*> (x .@ "Quantity")

instance Hashable ActiveTrustedSigners where

instance NFData ActiveTrustedSigners where
