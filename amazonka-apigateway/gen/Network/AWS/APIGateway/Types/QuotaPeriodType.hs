{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.QuotaPeriodType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.APIGateway.Types.QuotaPeriodType (
  QuotaPeriodType (
    ..
    , Day
    , Month
    , Week
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data QuotaPeriodType = QuotaPeriodType' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Day :: QuotaPeriodType
pattern Day = QuotaPeriodType' "DAY"

pattern Month :: QuotaPeriodType
pattern Month = QuotaPeriodType' "MONTH"

pattern Week :: QuotaPeriodType
pattern Week = QuotaPeriodType' "WEEK"

{-# COMPLETE
  Day,
  Month,
  Week,
  QuotaPeriodType' #-}

instance FromText QuotaPeriodType where
    parser = (QuotaPeriodType' . mk) <$> takeText

instance ToText QuotaPeriodType where
    toText (QuotaPeriodType' ci) = original ci

-- | Represents an enum of /known/ $QuotaPeriodType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum QuotaPeriodType where
    toEnum i = case i of
        0 -> Day
        1 -> Month
        2 -> Week
        _ -> (error . showText) $ "Unknown index for QuotaPeriodType: " <> toText i
    fromEnum x = case x of
        Day -> 0
        Month -> 1
        Week -> 2
        QuotaPeriodType' name -> (error . showText) $ "Unknown QuotaPeriodType: " <> original name

-- | Represents the bounds of /known/ $QuotaPeriodType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded QuotaPeriodType where
    minBound = Day
    maxBound = Week

instance Hashable     QuotaPeriodType
instance NFData       QuotaPeriodType
instance ToByteString QuotaPeriodType
instance ToQuery      QuotaPeriodType
instance ToHeader     QuotaPeriodType

instance ToJSON QuotaPeriodType where
    toJSON = toJSONText

instance FromJSON QuotaPeriodType where
    parseJSON = parseJSONText "QuotaPeriodType"
