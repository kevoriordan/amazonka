{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.S3EncryptionMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.S3EncryptionMode (
  S3EncryptionMode (
    ..
    , Disabled
    , SseKMS
    , SseS3
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data S3EncryptionMode = S3EncryptionMode' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern Disabled :: S3EncryptionMode
pattern Disabled = S3EncryptionMode' "DISABLED"

pattern SseKMS :: S3EncryptionMode
pattern SseKMS = S3EncryptionMode' "SSE-KMS"

pattern SseS3 :: S3EncryptionMode
pattern SseS3 = S3EncryptionMode' "SSE-S3"

{-# COMPLETE
  Disabled,
  SseKMS,
  SseS3,
  S3EncryptionMode' #-}

instance FromText S3EncryptionMode where
    parser = (S3EncryptionMode' . mk) <$> takeText

instance ToText S3EncryptionMode where
    toText (S3EncryptionMode' ci) = original ci

-- | Represents an enum of /known/ $S3EncryptionMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum S3EncryptionMode where
    toEnum i = case i of
        0 -> Disabled
        1 -> SseKMS
        2 -> SseS3
        _ -> (error . showText) $ "Unknown index for S3EncryptionMode: " <> toText i
    fromEnum x = case x of
        Disabled -> 0
        SseKMS -> 1
        SseS3 -> 2
        S3EncryptionMode' name -> (error . showText) $ "Unknown S3EncryptionMode: " <> original name

-- | Represents the bounds of /known/ $S3EncryptionMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded S3EncryptionMode where
    minBound = Disabled
    maxBound = SseS3

instance Hashable     S3EncryptionMode
instance NFData       S3EncryptionMode
instance ToByteString S3EncryptionMode
instance ToQuery      S3EncryptionMode
instance ToHeader     S3EncryptionMode

instance ToJSON S3EncryptionMode where
    toJSON = toJSONText

instance FromJSON S3EncryptionMode where
    parseJSON = parseJSONText "S3EncryptionMode"
