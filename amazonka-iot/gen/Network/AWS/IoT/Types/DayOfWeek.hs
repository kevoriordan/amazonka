{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.DayOfWeek
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.DayOfWeek (
  DayOfWeek (
    ..
    , Fri
    , Mon
    , Sat
    , Sun
    , Thu
    , Tue
    , Wed
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DayOfWeek = DayOfWeek' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern Fri :: DayOfWeek
pattern Fri = DayOfWeek' "FRI"

pattern Mon :: DayOfWeek
pattern Mon = DayOfWeek' "MON"

pattern Sat :: DayOfWeek
pattern Sat = DayOfWeek' "SAT"

pattern Sun :: DayOfWeek
pattern Sun = DayOfWeek' "SUN"

pattern Thu :: DayOfWeek
pattern Thu = DayOfWeek' "THU"

pattern Tue :: DayOfWeek
pattern Tue = DayOfWeek' "TUE"

pattern Wed :: DayOfWeek
pattern Wed = DayOfWeek' "WED"

{-# COMPLETE
  Fri,
  Mon,
  Sat,
  Sun,
  Thu,
  Tue,
  Wed,
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
        0 -> Fri
        1 -> Mon
        2 -> Sat
        3 -> Sun
        4 -> Thu
        5 -> Tue
        6 -> Wed
        _ -> (error . showText) $ "Unknown index for DayOfWeek: " <> toText i
    fromEnum x = case x of
        Fri -> 0
        Mon -> 1
        Sat -> 2
        Sun -> 3
        Thu -> 4
        Tue -> 5
        Wed -> 6
        DayOfWeek' name -> (error . showText) $ "Unknown DayOfWeek: " <> original name

-- | Represents the bounds of /known/ $DayOfWeek.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DayOfWeek where
    minBound = Fri
    maxBound = Wed

instance Hashable     DayOfWeek
instance NFData       DayOfWeek
instance ToByteString DayOfWeek
instance ToQuery      DayOfWeek
instance ToHeader     DayOfWeek

instance ToJSON DayOfWeek where
    toJSON = toJSONText

instance FromJSON DayOfWeek where
    parseJSON = parseJSONText "DayOfWeek"
