{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReportInterval
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.BusinessReportInterval (
  BusinessReportInterval (
    ..
    , OneDay
    , OneWeek
    , ThirtyDays
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data BusinessReportInterval = BusinessReportInterval' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern OneDay :: BusinessReportInterval
pattern OneDay = BusinessReportInterval' "ONE_DAY"

pattern OneWeek :: BusinessReportInterval
pattern OneWeek = BusinessReportInterval' "ONE_WEEK"

pattern ThirtyDays :: BusinessReportInterval
pattern ThirtyDays = BusinessReportInterval' "THIRTY_DAYS"

{-# COMPLETE
  OneDay,
  OneWeek,
  ThirtyDays,
  BusinessReportInterval' #-}

instance FromText BusinessReportInterval where
    parser = (BusinessReportInterval' . mk) <$> takeText

instance ToText BusinessReportInterval where
    toText (BusinessReportInterval' ci) = original ci

-- | Represents an enum of /known/ $BusinessReportInterval.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BusinessReportInterval where
    toEnum i = case i of
        0 -> OneDay
        1 -> OneWeek
        2 -> ThirtyDays
        _ -> (error . showText) $ "Unknown index for BusinessReportInterval: " <> toText i
    fromEnum x = case x of
        OneDay -> 0
        OneWeek -> 1
        ThirtyDays -> 2
        BusinessReportInterval' name -> (error . showText) $ "Unknown BusinessReportInterval: " <> original name

-- | Represents the bounds of /known/ $BusinessReportInterval.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BusinessReportInterval where
    minBound = OneDay
    maxBound = ThirtyDays

instance Hashable     BusinessReportInterval
instance NFData       BusinessReportInterval
instance ToByteString BusinessReportInterval
instance ToQuery      BusinessReportInterval
instance ToHeader     BusinessReportInterval

instance ToJSON BusinessReportInterval where
    toJSON = toJSONText

instance FromJSON BusinessReportInterval where
    parseJSON = parseJSONText "BusinessReportInterval"
