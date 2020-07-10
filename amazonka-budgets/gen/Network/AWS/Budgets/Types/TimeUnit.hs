{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.TimeUnit
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Budgets.Types.TimeUnit (
  TimeUnit (
    ..
    , Annually
    , Daily
    , Monthly
    , Quarterly
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The time unit of the budget, such as MONTHLY or QUARTERLY.
--
--
data TimeUnit = TimeUnit' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern Annually :: TimeUnit
pattern Annually = TimeUnit' "ANNUALLY"

pattern Daily :: TimeUnit
pattern Daily = TimeUnit' "DAILY"

pattern Monthly :: TimeUnit
pattern Monthly = TimeUnit' "MONTHLY"

pattern Quarterly :: TimeUnit
pattern Quarterly = TimeUnit' "QUARTERLY"

{-# COMPLETE
  Annually,
  Daily,
  Monthly,
  Quarterly,
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
        0 -> Annually
        1 -> Daily
        2 -> Monthly
        3 -> Quarterly
        _ -> (error . showText) $ "Unknown index for TimeUnit: " <> toText i
    fromEnum x = case x of
        Annually -> 0
        Daily -> 1
        Monthly -> 2
        Quarterly -> 3
        TimeUnit' name -> (error . showText) $ "Unknown TimeUnit: " <> original name

-- | Represents the bounds of /known/ $TimeUnit.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TimeUnit where
    minBound = Annually
    maxBound = Quarterly

instance Hashable     TimeUnit
instance NFData       TimeUnit
instance ToByteString TimeUnit
instance ToQuery      TimeUnit
instance ToHeader     TimeUnit

instance ToJSON TimeUnit where
    toJSON = toJSONText

instance FromJSON TimeUnit where
    parseJSON = parseJSONText "TimeUnit"
