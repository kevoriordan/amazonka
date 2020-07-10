{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.SSEType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.SSEType (
  SSEType (
    ..
    , AES256
    , KMS
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SSEType = SSEType' (CI Text)
                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                           Generic)

pattern AES256 :: SSEType
pattern AES256 = SSEType' "AES256"

pattern KMS :: SSEType
pattern KMS = SSEType' "KMS"

{-# COMPLETE
  AES256,
  KMS,
  SSEType' #-}

instance FromText SSEType where
    parser = (SSEType' . mk) <$> takeText

instance ToText SSEType where
    toText (SSEType' ci) = original ci

-- | Represents an enum of /known/ $SSEType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SSEType where
    toEnum i = case i of
        0 -> AES256
        1 -> KMS
        _ -> (error . showText) $ "Unknown index for SSEType: " <> toText i
    fromEnum x = case x of
        AES256 -> 0
        KMS -> 1
        SSEType' name -> (error . showText) $ "Unknown SSEType: " <> original name

-- | Represents the bounds of /known/ $SSEType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SSEType where
    minBound = AES256
    maxBound = KMS

instance Hashable     SSEType
instance NFData       SSEType
instance ToByteString SSEType
instance ToQuery      SSEType
instance ToHeader     SSEType

instance ToJSON SSEType where
    toJSON = toJSONText

instance FromJSON SSEType where
    parseJSON = parseJSONText "SSEType"
