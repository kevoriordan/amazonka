{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.SentimentType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.SentimentType (
  SentimentType (
    ..
    , Mixed
    , Negative
    , Neutral
    , Positive
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SentimentType = SentimentType' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern Mixed :: SentimentType
pattern Mixed = SentimentType' "MIXED"

pattern Negative :: SentimentType
pattern Negative = SentimentType' "NEGATIVE"

pattern Neutral :: SentimentType
pattern Neutral = SentimentType' "NEUTRAL"

pattern Positive :: SentimentType
pattern Positive = SentimentType' "POSITIVE"

{-# COMPLETE
  Mixed,
  Negative,
  Neutral,
  Positive,
  SentimentType' #-}

instance FromText SentimentType where
    parser = (SentimentType' . mk) <$> takeText

instance ToText SentimentType where
    toText (SentimentType' ci) = original ci

-- | Represents an enum of /known/ $SentimentType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SentimentType where
    toEnum i = case i of
        0 -> Mixed
        1 -> Negative
        2 -> Neutral
        3 -> Positive
        _ -> (error . showText) $ "Unknown index for SentimentType: " <> toText i
    fromEnum x = case x of
        Mixed -> 0
        Negative -> 1
        Neutral -> 2
        Positive -> 3
        SentimentType' name -> (error . showText) $ "Unknown SentimentType: " <> original name

-- | Represents the bounds of /known/ $SentimentType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SentimentType where
    minBound = Mixed
    maxBound = Positive

instance Hashable     SentimentType
instance NFData       SentimentType
instance ToByteString SentimentType
instance ToQuery      SentimentType
instance ToHeader     SentimentType

instance FromJSON SentimentType where
    parseJSON = parseJSONText "SentimentType"
