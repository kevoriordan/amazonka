{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Signer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.Signer where

import Network.AWS.CloudFront.Types.KeyPairIds
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that lists the AWS accounts that were included in the @TrustedSigners@ complex type, as well as their active CloudFront key pair IDs, if any. 
--
--
--
-- /See:/ 'signer' smart constructor.
data Signer = Signer'{_sAWSAccountNumber ::
                      !(Maybe Text),
                      _sKeyPairIds :: !(Maybe KeyPairIds)}
                deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Signer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sAWSAccountNumber' - An AWS account that is included in the @TrustedSigners@ complex type for this distribution. Valid values include:     * @self@ , which is the AWS account used to create the distribution.     * An AWS account number.
--
-- * 'sKeyPairIds' - A complex type that lists the active CloudFront key pairs, if any, that are associated with @AwsAccountNumber@ .
signer
    :: Signer
signer
  = Signer'{_sAWSAccountNumber = Nothing,
            _sKeyPairIds = Nothing}

-- | An AWS account that is included in the @TrustedSigners@ complex type for this distribution. Valid values include:     * @self@ , which is the AWS account used to create the distribution.     * An AWS account number.
sAWSAccountNumber :: Lens' Signer (Maybe Text)
sAWSAccountNumber = lens _sAWSAccountNumber (\ s a -> s{_sAWSAccountNumber = a})

-- | A complex type that lists the active CloudFront key pairs, if any, that are associated with @AwsAccountNumber@ .
sKeyPairIds :: Lens' Signer (Maybe KeyPairIds)
sKeyPairIds = lens _sKeyPairIds (\ s a -> s{_sKeyPairIds = a})

instance FromXML Signer where
        parseXML x
          = Signer' <$>
              (x .@? "AwsAccountNumber") <*> (x .@? "KeyPairIds")

instance Hashable Signer where

instance NFData Signer where
