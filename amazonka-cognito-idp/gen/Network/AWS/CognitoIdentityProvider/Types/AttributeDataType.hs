{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AttributeDataType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.AttributeDataType (
  AttributeDataType (
    ..
    , Boolean
    , DateTime
    , Number
    , String
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AttributeDataType = AttributeDataType' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern Boolean :: AttributeDataType
pattern Boolean = AttributeDataType' "Boolean"

pattern DateTime :: AttributeDataType
pattern DateTime = AttributeDataType' "DateTime"

pattern Number :: AttributeDataType
pattern Number = AttributeDataType' "Number"

pattern String :: AttributeDataType
pattern String = AttributeDataType' "String"

{-# COMPLETE
  Boolean,
  DateTime,
  Number,
  String,
  AttributeDataType' #-}

instance FromText AttributeDataType where
    parser = (AttributeDataType' . mk) <$> takeText

instance ToText AttributeDataType where
    toText (AttributeDataType' ci) = original ci

-- | Represents an enum of /known/ $AttributeDataType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AttributeDataType where
    toEnum i = case i of
        0 -> Boolean
        1 -> DateTime
        2 -> Number
        3 -> String
        _ -> (error . showText) $ "Unknown index for AttributeDataType: " <> toText i
    fromEnum x = case x of
        Boolean -> 0
        DateTime -> 1
        Number -> 2
        String -> 3
        AttributeDataType' name -> (error . showText) $ "Unknown AttributeDataType: " <> original name

-- | Represents the bounds of /known/ $AttributeDataType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AttributeDataType where
    minBound = Boolean
    maxBound = String

instance Hashable     AttributeDataType
instance NFData       AttributeDataType
instance ToByteString AttributeDataType
instance ToQuery      AttributeDataType
instance ToHeader     AttributeDataType

instance ToJSON AttributeDataType where
    toJSON = toJSONText

instance FromJSON AttributeDataType where
    parseJSON = parseJSONText "AttributeDataType"
