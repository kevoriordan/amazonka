{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.EncodingTypeValue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.EncodingTypeValue (
  EncodingTypeValue (
    ..
    , Plain
    , PlainDictionary
    , RleDictionary
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EncodingTypeValue = EncodingTypeValue' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern Plain :: EncodingTypeValue
pattern Plain = EncodingTypeValue' "plain"

pattern PlainDictionary :: EncodingTypeValue
pattern PlainDictionary = EncodingTypeValue' "plain-dictionary"

pattern RleDictionary :: EncodingTypeValue
pattern RleDictionary = EncodingTypeValue' "rle-dictionary"

{-# COMPLETE
  Plain,
  PlainDictionary,
  RleDictionary,
  EncodingTypeValue' #-}

instance FromText EncodingTypeValue where
    parser = (EncodingTypeValue' . mk) <$> takeText

instance ToText EncodingTypeValue where
    toText (EncodingTypeValue' ci) = original ci

-- | Represents an enum of /known/ $EncodingTypeValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EncodingTypeValue where
    toEnum i = case i of
        0 -> Plain
        1 -> PlainDictionary
        2 -> RleDictionary
        _ -> (error . showText) $ "Unknown index for EncodingTypeValue: " <> toText i
    fromEnum x = case x of
        Plain -> 0
        PlainDictionary -> 1
        RleDictionary -> 2
        EncodingTypeValue' name -> (error . showText) $ "Unknown EncodingTypeValue: " <> original name

-- | Represents the bounds of /known/ $EncodingTypeValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EncodingTypeValue where
    minBound = Plain
    maxBound = RleDictionary

instance Hashable     EncodingTypeValue
instance NFData       EncodingTypeValue
instance ToByteString EncodingTypeValue
instance ToQuery      EncodingTypeValue
instance ToHeader     EncodingTypeValue

instance ToJSON EncodingTypeValue where
    toJSON = toJSONText

instance FromJSON EncodingTypeValue where
    parseJSON = parseJSONText "EncodingTypeValue"
