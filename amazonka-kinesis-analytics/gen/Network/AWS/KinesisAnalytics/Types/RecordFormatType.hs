{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.RecordFormatType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.RecordFormatType (
  RecordFormatType (
    ..
    , CSV
    , JSON
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RecordFormatType = RecordFormatType' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern CSV :: RecordFormatType
pattern CSV = RecordFormatType' "CSV"

pattern JSON :: RecordFormatType
pattern JSON = RecordFormatType' "JSON"

{-# COMPLETE
  CSV,
  JSON,
  RecordFormatType' #-}

instance FromText RecordFormatType where
    parser = (RecordFormatType' . mk) <$> takeText

instance ToText RecordFormatType where
    toText (RecordFormatType' ci) = original ci

-- | Represents an enum of /known/ $RecordFormatType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RecordFormatType where
    toEnum i = case i of
        0 -> CSV
        1 -> JSON
        _ -> (error . showText) $ "Unknown index for RecordFormatType: " <> toText i
    fromEnum x = case x of
        CSV -> 0
        JSON -> 1
        RecordFormatType' name -> (error . showText) $ "Unknown RecordFormatType: " <> original name

-- | Represents the bounds of /known/ $RecordFormatType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RecordFormatType where
    minBound = CSV
    maxBound = JSON

instance Hashable     RecordFormatType
instance NFData       RecordFormatType
instance ToByteString RecordFormatType
instance ToQuery      RecordFormatType
instance ToHeader     RecordFormatType

instance ToJSON RecordFormatType where
    toJSON = toJSONText

instance FromJSON RecordFormatType where
    parseJSON = parseJSONText "RecordFormatType"
