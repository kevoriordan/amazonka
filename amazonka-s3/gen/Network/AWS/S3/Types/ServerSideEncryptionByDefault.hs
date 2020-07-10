{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ServerSideEncryptionByDefault
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.ServerSideEncryptionByDefault where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ServerSideEncryption

-- | Describes the default server-side encryption to apply to new objects in the bucket. If a PUT Object request doesn't specify any server-side encryption, this default encryption will be applied. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTencryption.html PUT Bucket encryption> in the /Amazon Simple Storage Service API Reference/ .
--
--
--
-- /See:/ 'serverSideEncryptionByDefault' smart constructor.
data ServerSideEncryptionByDefault = ServerSideEncryptionByDefault'{_ssebdKMSMasterKeyId
                                                                    ::
                                                                    !(Maybe
                                                                        (Sensitive
                                                                           Text)),
                                                                    _ssebdSSEAlgorithm
                                                                    ::
                                                                    !ServerSideEncryption}
                                       deriving (Eq, Show, Data, Typeable,
                                                 Generic)

-- | Creates a value of 'ServerSideEncryptionByDefault' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssebdKMSMasterKeyId' - KMS master key ID to use for the default encryption. This parameter is allowed if and only if @SSEAlgorithm@ is set to @aws:kms@ .
--
-- * 'ssebdSSEAlgorithm' - Server-side encryption algorithm to use for the default encryption.
serverSideEncryptionByDefault
    :: ServerSideEncryption -- ^ 'ssebdSSEAlgorithm'
    -> ServerSideEncryptionByDefault
serverSideEncryptionByDefault pSSEAlgorithm_
  = ServerSideEncryptionByDefault'{_ssebdKMSMasterKeyId
                                     = Nothing,
                                   _ssebdSSEAlgorithm = pSSEAlgorithm_}

-- | KMS master key ID to use for the default encryption. This parameter is allowed if and only if @SSEAlgorithm@ is set to @aws:kms@ .
ssebdKMSMasterKeyId :: Lens' ServerSideEncryptionByDefault (Maybe Text)
ssebdKMSMasterKeyId = lens _ssebdKMSMasterKeyId (\ s a -> s{_ssebdKMSMasterKeyId = a}) . mapping _Sensitive

-- | Server-side encryption algorithm to use for the default encryption.
ssebdSSEAlgorithm :: Lens' ServerSideEncryptionByDefault ServerSideEncryption
ssebdSSEAlgorithm = lens _ssebdSSEAlgorithm (\ s a -> s{_ssebdSSEAlgorithm = a})

instance FromXML ServerSideEncryptionByDefault where
        parseXML x
          = ServerSideEncryptionByDefault' <$>
              (x .@? "KMSMasterKeyID") <*> (x .@ "SSEAlgorithm")

instance Hashable ServerSideEncryptionByDefault where

instance NFData ServerSideEncryptionByDefault where

instance ToXML ServerSideEncryptionByDefault where
        toXML ServerSideEncryptionByDefault'{..}
          = mconcat
              ["KMSMasterKeyID" @= _ssebdKMSMasterKeyId,
               "SSEAlgorithm" @= _ssebdSSEAlgorithm]
