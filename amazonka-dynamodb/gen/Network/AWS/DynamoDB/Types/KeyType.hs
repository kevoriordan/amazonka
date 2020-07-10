{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.KeyType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.KeyType (
  KeyType (
    ..
    , Hash
    , Range
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data KeyType = KeyType' (CI Text)
                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                           Generic)

pattern Hash :: KeyType
pattern Hash = KeyType' "HASH"

pattern Range :: KeyType
pattern Range = KeyType' "RANGE"

{-# COMPLETE
  Hash,
  Range,
  KeyType' #-}

instance FromText KeyType where
    parser = (KeyType' . mk) <$> takeText

instance ToText KeyType where
    toText (KeyType' ci) = original ci

-- | Represents an enum of /known/ $KeyType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum KeyType where
    toEnum i = case i of
        0 -> Hash
        1 -> Range
        _ -> (error . showText) $ "Unknown index for KeyType: " <> toText i
    fromEnum x = case x of
        Hash -> 0
        Range -> 1
        KeyType' name -> (error . showText) $ "Unknown KeyType: " <> original name

-- | Represents the bounds of /known/ $KeyType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded KeyType where
    minBound = Hash
    maxBound = Range

instance Hashable     KeyType
instance NFData       KeyType
instance ToByteString KeyType
instance ToQuery      KeyType
instance ToHeader     KeyType

instance ToJSON KeyType where
    toJSON = toJSONText

instance FromJSON KeyType where
    parseJSON = parseJSONText "KeyType"
