{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafEncryptionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.CmafEncryptionType (
  CmafEncryptionType (
    ..
    , AESCtr
    , SampleAES
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Specify the encryption scheme that you want the service to use when encrypting your CMAF segments. Choose AES-CBC subsample (SAMPLE-AES) or AES_CTR (AES-CTR).
data CmafEncryptionType = CmafEncryptionType' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern AESCtr :: CmafEncryptionType
pattern AESCtr = CmafEncryptionType' "AES_CTR"

pattern SampleAES :: CmafEncryptionType
pattern SampleAES = CmafEncryptionType' "SAMPLE_AES"

{-# COMPLETE
  AESCtr,
  SampleAES,
  CmafEncryptionType' #-}

instance FromText CmafEncryptionType where
    parser = (CmafEncryptionType' . mk) <$> takeText

instance ToText CmafEncryptionType where
    toText (CmafEncryptionType' ci) = original ci

-- | Represents an enum of /known/ $CmafEncryptionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CmafEncryptionType where
    toEnum i = case i of
        0 -> AESCtr
        1 -> SampleAES
        _ -> (error . showText) $ "Unknown index for CmafEncryptionType: " <> toText i
    fromEnum x = case x of
        AESCtr -> 0
        SampleAES -> 1
        CmafEncryptionType' name -> (error . showText) $ "Unknown CmafEncryptionType: " <> original name

-- | Represents the bounds of /known/ $CmafEncryptionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CmafEncryptionType where
    minBound = AESCtr
    maxBound = SampleAES

instance Hashable     CmafEncryptionType
instance NFData       CmafEncryptionType
instance ToByteString CmafEncryptionType
instance ToQuery      CmafEncryptionType
instance ToHeader     CmafEncryptionType

instance ToJSON CmafEncryptionType where
    toJSON = toJSONText

instance FromJSON CmafEncryptionType where
    parseJSON = parseJSONText "CmafEncryptionType"
