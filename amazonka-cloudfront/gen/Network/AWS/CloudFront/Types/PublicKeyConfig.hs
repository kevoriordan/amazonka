{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.PublicKeyConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.PublicKeyConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a public key you add to CloudFront to use with features like field-level encryption.
--
--
--
-- /See:/ 'publicKeyConfig' smart constructor.
data PublicKeyConfig = PublicKeyConfig'{_pkcComment
                                        :: !(Maybe Text),
                                        _pkcCallerReference :: !Text,
                                        _pkcName :: !Text,
                                        _pkcEncodedKey :: !Text}
                         deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PublicKeyConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pkcComment' - An optional comment about a public key.
--
-- * 'pkcCallerReference' - A unique number that ensures that the request can't be replayed.
--
-- * 'pkcName' - The name for a public key you add to CloudFront to use with features like field-level encryption.
--
-- * 'pkcEncodedKey' - The encoded public key that you want to add to CloudFront to use with features like field-level encryption.
publicKeyConfig
    :: Text -- ^ 'pkcCallerReference'
    -> Text -- ^ 'pkcName'
    -> Text -- ^ 'pkcEncodedKey'
    -> PublicKeyConfig
publicKeyConfig pCallerReference_ pName_ pEncodedKey_
  = PublicKeyConfig'{_pkcComment = Nothing,
                     _pkcCallerReference = pCallerReference_,
                     _pkcName = pName_, _pkcEncodedKey = pEncodedKey_}

-- | An optional comment about a public key.
pkcComment :: Lens' PublicKeyConfig (Maybe Text)
pkcComment = lens _pkcComment (\ s a -> s{_pkcComment = a})

-- | A unique number that ensures that the request can't be replayed.
pkcCallerReference :: Lens' PublicKeyConfig Text
pkcCallerReference = lens _pkcCallerReference (\ s a -> s{_pkcCallerReference = a})

-- | The name for a public key you add to CloudFront to use with features like field-level encryption.
pkcName :: Lens' PublicKeyConfig Text
pkcName = lens _pkcName (\ s a -> s{_pkcName = a})

-- | The encoded public key that you want to add to CloudFront to use with features like field-level encryption.
pkcEncodedKey :: Lens' PublicKeyConfig Text
pkcEncodedKey = lens _pkcEncodedKey (\ s a -> s{_pkcEncodedKey = a})

instance FromXML PublicKeyConfig where
        parseXML x
          = PublicKeyConfig' <$>
              (x .@? "Comment") <*> (x .@ "CallerReference") <*>
                (x .@ "Name")
                <*> (x .@ "EncodedKey")

instance Hashable PublicKeyConfig where

instance NFData PublicKeyConfig where

instance ToXML PublicKeyConfig where
        toXML PublicKeyConfig'{..}
          = mconcat
              ["Comment" @= _pkcComment,
               "CallerReference" @= _pkcCallerReference,
               "Name" @= _pkcName, "EncodedKey" @= _pkcEncodedKey]
