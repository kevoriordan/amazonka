{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.FacetAttributeType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.FacetAttributeType (
  FacetAttributeType (
    ..
    , Binary
    , Boolean
    , Datetime
    , Number
    , String
    , Variant
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data FacetAttributeType = FacetAttributeType' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern Binary :: FacetAttributeType
pattern Binary = FacetAttributeType' "BINARY"

pattern Boolean :: FacetAttributeType
pattern Boolean = FacetAttributeType' "BOOLEAN"

pattern Datetime :: FacetAttributeType
pattern Datetime = FacetAttributeType' "DATETIME"

pattern Number :: FacetAttributeType
pattern Number = FacetAttributeType' "NUMBER"

pattern String :: FacetAttributeType
pattern String = FacetAttributeType' "STRING"

pattern Variant :: FacetAttributeType
pattern Variant = FacetAttributeType' "VARIANT"

{-# COMPLETE
  Binary,
  Boolean,
  Datetime,
  Number,
  String,
  Variant,
  FacetAttributeType' #-}

instance FromText FacetAttributeType where
    parser = (FacetAttributeType' . mk) <$> takeText

instance ToText FacetAttributeType where
    toText (FacetAttributeType' ci) = original ci

-- | Represents an enum of /known/ $FacetAttributeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FacetAttributeType where
    toEnum i = case i of
        0 -> Binary
        1 -> Boolean
        2 -> Datetime
        3 -> Number
        4 -> String
        5 -> Variant
        _ -> (error . showText) $ "Unknown index for FacetAttributeType: " <> toText i
    fromEnum x = case x of
        Binary -> 0
        Boolean -> 1
        Datetime -> 2
        Number -> 3
        String -> 4
        Variant -> 5
        FacetAttributeType' name -> (error . showText) $ "Unknown FacetAttributeType: " <> original name

-- | Represents the bounds of /known/ $FacetAttributeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FacetAttributeType where
    minBound = Binary
    maxBound = Variant

instance Hashable     FacetAttributeType
instance NFData       FacetAttributeType
instance ToByteString FacetAttributeType
instance ToQuery      FacetAttributeType
instance ToHeader     FacetAttributeType

instance ToJSON FacetAttributeType where
    toJSON = toJSONText

instance FromJSON FacetAttributeType where
    parseJSON = parseJSONText "FacetAttributeType"
