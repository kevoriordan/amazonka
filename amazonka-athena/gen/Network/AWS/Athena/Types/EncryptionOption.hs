{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.EncryptionOption
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Athena.Types.EncryptionOption (
  EncryptionOption (
    ..
    , CseKMS
    , SseKMS
    , SseS3
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EncryptionOption = EncryptionOption' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern CseKMS :: EncryptionOption
pattern CseKMS = EncryptionOption' "CSE_KMS"

pattern SseKMS :: EncryptionOption
pattern SseKMS = EncryptionOption' "SSE_KMS"

pattern SseS3 :: EncryptionOption
pattern SseS3 = EncryptionOption' "SSE_S3"

{-# COMPLETE
  CseKMS,
  SseKMS,
  SseS3,
  EncryptionOption' #-}

instance FromText EncryptionOption where
    parser = (EncryptionOption' . mk) <$> takeText

instance ToText EncryptionOption where
    toText (EncryptionOption' ci) = original ci

-- | Represents an enum of /known/ $EncryptionOption.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EncryptionOption where
    toEnum i = case i of
        0 -> CseKMS
        1 -> SseKMS
        2 -> SseS3
        _ -> (error . showText) $ "Unknown index for EncryptionOption: " <> toText i
    fromEnum x = case x of
        CseKMS -> 0
        SseKMS -> 1
        SseS3 -> 2
        EncryptionOption' name -> (error . showText) $ "Unknown EncryptionOption: " <> original name

-- | Represents the bounds of /known/ $EncryptionOption.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EncryptionOption where
    minBound = CseKMS
    maxBound = SseS3

instance Hashable     EncryptionOption
instance NFData       EncryptionOption
instance ToByteString EncryptionOption
instance ToQuery      EncryptionOption
instance ToHeader     EncryptionOption

instance ToJSON EncryptionOption where
    toJSON = toJSONText

instance FromJSON EncryptionOption where
    parseJSON = parseJSONText "EncryptionOption"
