{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.EncryptionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.XRay.Types.EncryptionType (
  EncryptionType (
    ..
    , KMS
    , None
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EncryptionType = EncryptionType' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern KMS :: EncryptionType
pattern KMS = EncryptionType' "KMS"

pattern None :: EncryptionType
pattern None = EncryptionType' "NONE"

{-# COMPLETE
  KMS,
  None,
  EncryptionType' #-}

instance FromText EncryptionType where
    parser = (EncryptionType' . mk) <$> takeText

instance ToText EncryptionType where
    toText (EncryptionType' ci) = original ci

-- | Represents an enum of /known/ $EncryptionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EncryptionType where
    toEnum i = case i of
        0 -> KMS
        1 -> None
        _ -> (error . showText) $ "Unknown index for EncryptionType: " <> toText i
    fromEnum x = case x of
        KMS -> 0
        None -> 1
        EncryptionType' name -> (error . showText) $ "Unknown EncryptionType: " <> original name

-- | Represents the bounds of /known/ $EncryptionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EncryptionType where
    minBound = KMS
    maxBound = None

instance Hashable     EncryptionType
instance NFData       EncryptionType
instance ToByteString EncryptionType
instance ToQuery      EncryptionType
instance ToHeader     EncryptionType

instance ToJSON EncryptionType where
    toJSON = toJSONText

instance FromJSON EncryptionType where
    parseJSON = parseJSONText "EncryptionType"
