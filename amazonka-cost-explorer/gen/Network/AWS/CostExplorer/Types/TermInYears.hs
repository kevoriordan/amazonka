{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.TermInYears
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.TermInYears (
  TermInYears (
    ..
    , OneYear
    , ThreeYears
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TermInYears = TermInYears' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern OneYear :: TermInYears
pattern OneYear = TermInYears' "ONE_YEAR"

pattern ThreeYears :: TermInYears
pattern ThreeYears = TermInYears' "THREE_YEARS"

{-# COMPLETE
  OneYear,
  ThreeYears,
  TermInYears' #-}

instance FromText TermInYears where
    parser = (TermInYears' . mk) <$> takeText

instance ToText TermInYears where
    toText (TermInYears' ci) = original ci

-- | Represents an enum of /known/ $TermInYears.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TermInYears where
    toEnum i = case i of
        0 -> OneYear
        1 -> ThreeYears
        _ -> (error . showText) $ "Unknown index for TermInYears: " <> toText i
    fromEnum x = case x of
        OneYear -> 0
        ThreeYears -> 1
        TermInYears' name -> (error . showText) $ "Unknown TermInYears: " <> original name

-- | Represents the bounds of /known/ $TermInYears.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TermInYears where
    minBound = OneYear
    maxBound = ThreeYears

instance Hashable     TermInYears
instance NFData       TermInYears
instance ToByteString TermInYears
instance ToQuery      TermInYears
instance ToHeader     TermInYears

instance ToJSON TermInYears where
    toJSON = toJSONText

instance FromJSON TermInYears where
    parseJSON = parseJSONText "TermInYears"
