{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.NoEncryptionConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.NoEncryptionConfig (
  NoEncryptionConfig (
    ..
    , NoEncryption
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data NoEncryptionConfig = NoEncryptionConfig' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern NoEncryption :: NoEncryptionConfig
pattern NoEncryption = NoEncryptionConfig' "NoEncryption"

{-# COMPLETE
  NoEncryption,
  NoEncryptionConfig' #-}

instance FromText NoEncryptionConfig where
    parser = (NoEncryptionConfig' . mk) <$> takeText

instance ToText NoEncryptionConfig where
    toText (NoEncryptionConfig' ci) = original ci

-- | Represents an enum of /known/ $NoEncryptionConfig.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum NoEncryptionConfig where
    toEnum i = case i of
        0 -> NoEncryption
        _ -> (error . showText) $ "Unknown index for NoEncryptionConfig: " <> toText i
    fromEnum x = case x of
        NoEncryption -> 0
        NoEncryptionConfig' name -> (error . showText) $ "Unknown NoEncryptionConfig: " <> original name

-- | Represents the bounds of /known/ $NoEncryptionConfig.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded NoEncryptionConfig where
    minBound = NoEncryption
    maxBound = NoEncryption

instance Hashable     NoEncryptionConfig
instance NFData       NoEncryptionConfig
instance ToByteString NoEncryptionConfig
instance ToQuery      NoEncryptionConfig
instance ToHeader     NoEncryptionConfig

instance ToJSON NoEncryptionConfig where
    toJSON = toJSONText

instance FromJSON NoEncryptionConfig where
    parseJSON = parseJSONText "NoEncryptionConfig"
