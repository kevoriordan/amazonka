{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.DayOfWeek
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MQ.Types.DayOfWeek (
  DayOfWeek (
    ..
    , Friday
    , Monday
    , Saturday
    , Sunday
    , Thursday
    , Tuesday
    , Wednesday
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DayOfWeek = DayOfWeek' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern Friday :: DayOfWeek
pattern Friday = DayOfWeek' "FRIDAY"

pattern Monday :: DayOfWeek
pattern Monday = DayOfWeek' "MONDAY"

pattern Saturday :: DayOfWeek
pattern Saturday = DayOfWeek' "SATURDAY"

pattern Sunday :: DayOfWeek
pattern Sunday = DayOfWeek' "SUNDAY"

pattern Thursday :: DayOfWeek
pattern Thursday = DayOfWeek' "THURSDAY"

pattern Tuesday :: DayOfWeek
pattern Tuesday = DayOfWeek' "TUESDAY"

pattern Wednesday :: DayOfWeek
pattern Wednesday = DayOfWeek' "WEDNESDAY"

{-# COMPLETE
  Friday,
  Monday,
  Saturday,
  Sunday,
  Thursday,
  Tuesday,
  Wednesday,
  DayOfWeek' #-}

instance FromText DayOfWeek where
    parser = (DayOfWeek' . mk) <$> takeText

instance ToText DayOfWeek where
    toText (DayOfWeek' ci) = original ci

-- | Represents an enum of /known/ $DayOfWeek.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DayOfWeek where
    toEnum i = case i of
        0 -> Friday
        1 -> Monday
        2 -> Saturday
        3 -> Sunday
        4 -> Thursday
        5 -> Tuesday
        6 -> Wednesday
        _ -> (error . showText) $ "Unknown index for DayOfWeek: " <> toText i
    fromEnum x = case x of
        Friday -> 0
        Monday -> 1
        Saturday -> 2
        Sunday -> 3
        Thursday -> 4
        Tuesday -> 5
        Wednesday -> 6
        DayOfWeek' name -> (error . showText) $ "Unknown DayOfWeek: " <> original name

-- | Represents the bounds of /known/ $DayOfWeek.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DayOfWeek where
    minBound = Friday
    maxBound = Wednesday

instance Hashable     DayOfWeek
instance NFData       DayOfWeek
instance ToByteString DayOfWeek
instance ToQuery      DayOfWeek
instance ToHeader     DayOfWeek

instance ToJSON DayOfWeek where
    toJSON = toJSONText

instance FromJSON DayOfWeek where
    parseJSON = parseJSONText "DayOfWeek"
