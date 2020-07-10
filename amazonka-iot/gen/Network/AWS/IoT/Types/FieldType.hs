{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.FieldType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.FieldType (
  FieldType (
    ..
    , Boolean
    , Number
    , String
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data FieldType = FieldType' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern Boolean :: FieldType
pattern Boolean = FieldType' "Boolean"

pattern Number :: FieldType
pattern Number = FieldType' "Number"

pattern String :: FieldType
pattern String = FieldType' "String"

{-# COMPLETE
  Boolean,
  Number,
  String,
  FieldType' #-}

instance FromText FieldType where
    parser = (FieldType' . mk) <$> takeText

instance ToText FieldType where
    toText (FieldType' ci) = original ci

-- | Represents an enum of /known/ $FieldType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FieldType where
    toEnum i = case i of
        0 -> Boolean
        1 -> Number
        2 -> String
        _ -> (error . showText) $ "Unknown index for FieldType: " <> toText i
    fromEnum x = case x of
        Boolean -> 0
        Number -> 1
        String -> 2
        FieldType' name -> (error . showText) $ "Unknown FieldType: " <> original name

-- | Represents the bounds of /known/ $FieldType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FieldType where
    minBound = Boolean
    maxBound = String

instance Hashable     FieldType
instance NFData       FieldType
instance ToByteString FieldType
instance ToQuery      FieldType
instance ToHeader     FieldType

instance ToJSON FieldType where
    toJSON = toJSONText

instance FromJSON FieldType where
    parseJSON = parseJSONText "FieldType"
