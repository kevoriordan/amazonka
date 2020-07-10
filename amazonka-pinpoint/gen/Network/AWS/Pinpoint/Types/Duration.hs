{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Duration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.Duration (
  Duration (
    ..
    , Day14
    , Day30
    , Day7
    , Hr24
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Duration = Duration' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern Day14 :: Duration
pattern Day14 = Duration' "DAY_14"

pattern Day30 :: Duration
pattern Day30 = Duration' "DAY_30"

pattern Day7 :: Duration
pattern Day7 = Duration' "DAY_7"

pattern Hr24 :: Duration
pattern Hr24 = Duration' "HR_24"

{-# COMPLETE
  Day14,
  Day30,
  Day7,
  Hr24,
  Duration' #-}

instance FromText Duration where
    parser = (Duration' . mk) <$> takeText

instance ToText Duration where
    toText (Duration' ci) = original ci

-- | Represents an enum of /known/ $Duration.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Duration where
    toEnum i = case i of
        0 -> Day14
        1 -> Day30
        2 -> Day7
        3 -> Hr24
        _ -> (error . showText) $ "Unknown index for Duration: " <> toText i
    fromEnum x = case x of
        Day14 -> 0
        Day30 -> 1
        Day7 -> 2
        Hr24 -> 3
        Duration' name -> (error . showText) $ "Unknown Duration: " <> original name

-- | Represents the bounds of /known/ $Duration.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Duration where
    minBound = Day14
    maxBound = Hr24

instance Hashable     Duration
instance NFData       Duration
instance ToByteString Duration
instance ToQuery      Duration
instance ToHeader     Duration

instance ToJSON Duration where
    toJSON = toJSONText

instance FromJSON Duration where
    parseJSON = parseJSONText "Duration"
