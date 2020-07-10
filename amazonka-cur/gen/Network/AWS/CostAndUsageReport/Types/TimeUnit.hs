{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.Types.TimeUnit
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostAndUsageReport.Types.TimeUnit (
  TimeUnit (
    ..
    , Daily
    , Hourly
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The length of time covered by the report. 
--
--
data TimeUnit = TimeUnit' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern Daily :: TimeUnit
pattern Daily = TimeUnit' "DAILY"

pattern Hourly :: TimeUnit
pattern Hourly = TimeUnit' "HOURLY"

{-# COMPLETE
  Daily,
  Hourly,
  TimeUnit' #-}

instance FromText TimeUnit where
    parser = (TimeUnit' . mk) <$> takeText

instance ToText TimeUnit where
    toText (TimeUnit' ci) = original ci

-- | Represents an enum of /known/ $TimeUnit.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TimeUnit where
    toEnum i = case i of
        0 -> Daily
        1 -> Hourly
        _ -> (error . showText) $ "Unknown index for TimeUnit: " <> toText i
    fromEnum x = case x of
        Daily -> 0
        Hourly -> 1
        TimeUnit' name -> (error . showText) $ "Unknown TimeUnit: " <> original name

-- | Represents the bounds of /known/ $TimeUnit.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TimeUnit where
    minBound = Daily
    maxBound = Hourly

instance Hashable     TimeUnit
instance NFData       TimeUnit
instance ToByteString TimeUnit
instance ToQuery      TimeUnit
instance ToHeader     TimeUnit

instance ToJSON TimeUnit where
    toJSON = toJSONText

instance FromJSON TimeUnit where
    parseJSON = parseJSONText "TimeUnit"
