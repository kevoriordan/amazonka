{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsItemDataType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.OpsItemDataType (
  OpsItemDataType (
    ..
    , SearchableString
    , String
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data OpsItemDataType = OpsItemDataType' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern SearchableString :: OpsItemDataType
pattern SearchableString = OpsItemDataType' "SearchableString"

pattern String :: OpsItemDataType
pattern String = OpsItemDataType' "String"

{-# COMPLETE
  SearchableString,
  String,
  OpsItemDataType' #-}

instance FromText OpsItemDataType where
    parser = (OpsItemDataType' . mk) <$> takeText

instance ToText OpsItemDataType where
    toText (OpsItemDataType' ci) = original ci

-- | Represents an enum of /known/ $OpsItemDataType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OpsItemDataType where
    toEnum i = case i of
        0 -> SearchableString
        1 -> String
        _ -> (error . showText) $ "Unknown index for OpsItemDataType: " <> toText i
    fromEnum x = case x of
        SearchableString -> 0
        String -> 1
        OpsItemDataType' name -> (error . showText) $ "Unknown OpsItemDataType: " <> original name

-- | Represents the bounds of /known/ $OpsItemDataType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OpsItemDataType where
    minBound = SearchableString
    maxBound = String

instance Hashable     OpsItemDataType
instance NFData       OpsItemDataType
instance ToByteString OpsItemDataType
instance ToQuery      OpsItemDataType
instance ToHeader     OpsItemDataType

instance ToJSON OpsItemDataType where
    toJSON = toJSONText

instance FromJSON OpsItemDataType where
    parseJSON = parseJSONText "OpsItemDataType"
