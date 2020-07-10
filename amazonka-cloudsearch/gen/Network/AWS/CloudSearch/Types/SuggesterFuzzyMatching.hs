{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.SuggesterFuzzyMatching
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types.SuggesterFuzzyMatching (
  SuggesterFuzzyMatching (
    ..
    , High
    , Low
    , None
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SuggesterFuzzyMatching = SuggesterFuzzyMatching' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern High :: SuggesterFuzzyMatching
pattern High = SuggesterFuzzyMatching' "high"

pattern Low :: SuggesterFuzzyMatching
pattern Low = SuggesterFuzzyMatching' "low"

pattern None :: SuggesterFuzzyMatching
pattern None = SuggesterFuzzyMatching' "none"

{-# COMPLETE
  High,
  Low,
  None,
  SuggesterFuzzyMatching' #-}

instance FromText SuggesterFuzzyMatching where
    parser = (SuggesterFuzzyMatching' . mk) <$> takeText

instance ToText SuggesterFuzzyMatching where
    toText (SuggesterFuzzyMatching' ci) = original ci

-- | Represents an enum of /known/ $SuggesterFuzzyMatching.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SuggesterFuzzyMatching where
    toEnum i = case i of
        0 -> High
        1 -> Low
        2 -> None
        _ -> (error . showText) $ "Unknown index for SuggesterFuzzyMatching: " <> toText i
    fromEnum x = case x of
        High -> 0
        Low -> 1
        None -> 2
        SuggesterFuzzyMatching' name -> (error . showText) $ "Unknown SuggesterFuzzyMatching: " <> original name

-- | Represents the bounds of /known/ $SuggesterFuzzyMatching.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SuggesterFuzzyMatching where
    minBound = High
    maxBound = None

instance Hashable     SuggesterFuzzyMatching
instance NFData       SuggesterFuzzyMatching
instance ToByteString SuggesterFuzzyMatching
instance ToQuery      SuggesterFuzzyMatching
instance ToHeader     SuggesterFuzzyMatching

instance FromXML SuggesterFuzzyMatching where
    parseXML = parseXMLText "SuggesterFuzzyMatching"
