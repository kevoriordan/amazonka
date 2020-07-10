{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ElasticsearchIndexRotationPeriod
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.ElasticsearchIndexRotationPeriod (
  ElasticsearchIndexRotationPeriod (
    ..
    , NoRotation
    , OneDay
    , OneHour
    , OneMonth
    , OneWeek
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ElasticsearchIndexRotationPeriod = ElasticsearchIndexRotationPeriod' (CI
                                                                             Text)
                                          deriving (Eq, Ord, Read, Show, Data,
                                                    Typeable, Generic)

pattern NoRotation :: ElasticsearchIndexRotationPeriod
pattern NoRotation = ElasticsearchIndexRotationPeriod' "NoRotation"

pattern OneDay :: ElasticsearchIndexRotationPeriod
pattern OneDay = ElasticsearchIndexRotationPeriod' "OneDay"

pattern OneHour :: ElasticsearchIndexRotationPeriod
pattern OneHour = ElasticsearchIndexRotationPeriod' "OneHour"

pattern OneMonth :: ElasticsearchIndexRotationPeriod
pattern OneMonth = ElasticsearchIndexRotationPeriod' "OneMonth"

pattern OneWeek :: ElasticsearchIndexRotationPeriod
pattern OneWeek = ElasticsearchIndexRotationPeriod' "OneWeek"

{-# COMPLETE
  NoRotation,
  OneDay,
  OneHour,
  OneMonth,
  OneWeek,
  ElasticsearchIndexRotationPeriod' #-}

instance FromText ElasticsearchIndexRotationPeriod where
    parser = (ElasticsearchIndexRotationPeriod' . mk) <$> takeText

instance ToText ElasticsearchIndexRotationPeriod where
    toText (ElasticsearchIndexRotationPeriod' ci) = original ci

-- | Represents an enum of /known/ $ElasticsearchIndexRotationPeriod.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ElasticsearchIndexRotationPeriod where
    toEnum i = case i of
        0 -> NoRotation
        1 -> OneDay
        2 -> OneHour
        3 -> OneMonth
        4 -> OneWeek
        _ -> (error . showText) $ "Unknown index for ElasticsearchIndexRotationPeriod: " <> toText i
    fromEnum x = case x of
        NoRotation -> 0
        OneDay -> 1
        OneHour -> 2
        OneMonth -> 3
        OneWeek -> 4
        ElasticsearchIndexRotationPeriod' name -> (error . showText) $ "Unknown ElasticsearchIndexRotationPeriod: " <> original name

-- | Represents the bounds of /known/ $ElasticsearchIndexRotationPeriod.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ElasticsearchIndexRotationPeriod where
    minBound = NoRotation
    maxBound = OneWeek

instance Hashable     ElasticsearchIndexRotationPeriod
instance NFData       ElasticsearchIndexRotationPeriod
instance ToByteString ElasticsearchIndexRotationPeriod
instance ToQuery      ElasticsearchIndexRotationPeriod
instance ToHeader     ElasticsearchIndexRotationPeriod

instance ToJSON ElasticsearchIndexRotationPeriod where
    toJSON = toJSONText

instance FromJSON ElasticsearchIndexRotationPeriod where
    parseJSON = parseJSONText "ElasticsearchIndexRotationPeriod"
