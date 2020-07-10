{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstancePaymentOption
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstancePaymentOption (
  ReservedElasticsearchInstancePaymentOption (
    ..
    , AllUpfront
    , NoUpfront
    , PartialUpfront
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ReservedElasticsearchInstancePaymentOption = ReservedElasticsearchInstancePaymentOption' (CI
                                                                                                 Text)
                                                    deriving (Eq, Ord, Read,
                                                              Show, Data,
                                                              Typeable, Generic)

pattern AllUpfront :: ReservedElasticsearchInstancePaymentOption
pattern AllUpfront = ReservedElasticsearchInstancePaymentOption' "ALL_UPFRONT"

pattern NoUpfront :: ReservedElasticsearchInstancePaymentOption
pattern NoUpfront = ReservedElasticsearchInstancePaymentOption' "NO_UPFRONT"

pattern PartialUpfront :: ReservedElasticsearchInstancePaymentOption
pattern PartialUpfront = ReservedElasticsearchInstancePaymentOption' "PARTIAL_UPFRONT"

{-# COMPLETE
  AllUpfront,
  NoUpfront,
  PartialUpfront,
  ReservedElasticsearchInstancePaymentOption' #-}

instance FromText ReservedElasticsearchInstancePaymentOption where
    parser = (ReservedElasticsearchInstancePaymentOption' . mk) <$> takeText

instance ToText ReservedElasticsearchInstancePaymentOption where
    toText (ReservedElasticsearchInstancePaymentOption' ci) = original ci

-- | Represents an enum of /known/ $ReservedElasticsearchInstancePaymentOption.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReservedElasticsearchInstancePaymentOption where
    toEnum i = case i of
        0 -> AllUpfront
        1 -> NoUpfront
        2 -> PartialUpfront
        _ -> (error . showText) $ "Unknown index for ReservedElasticsearchInstancePaymentOption: " <> toText i
    fromEnum x = case x of
        AllUpfront -> 0
        NoUpfront -> 1
        PartialUpfront -> 2
        ReservedElasticsearchInstancePaymentOption' name -> (error . showText) $ "Unknown ReservedElasticsearchInstancePaymentOption: " <> original name

-- | Represents the bounds of /known/ $ReservedElasticsearchInstancePaymentOption.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReservedElasticsearchInstancePaymentOption where
    minBound = AllUpfront
    maxBound = PartialUpfront

instance Hashable     ReservedElasticsearchInstancePaymentOption
instance NFData       ReservedElasticsearchInstancePaymentOption
instance ToByteString ReservedElasticsearchInstancePaymentOption
instance ToQuery      ReservedElasticsearchInstancePaymentOption
instance ToHeader     ReservedElasticsearchInstancePaymentOption

instance FromJSON ReservedElasticsearchInstancePaymentOption where
    parseJSON = parseJSONText "ReservedElasticsearchInstancePaymentOption"
