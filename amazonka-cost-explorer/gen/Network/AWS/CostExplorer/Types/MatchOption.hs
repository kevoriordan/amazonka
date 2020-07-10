{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.MatchOption
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.MatchOption (
  MatchOption (
    ..
    , CaseInsensitive
    , CaseSensitive
    , Contains
    , EndsWith
    , Equals
    , StartsWith
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MatchOption = MatchOption' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern CaseInsensitive :: MatchOption
pattern CaseInsensitive = MatchOption' "CASE_INSENSITIVE"

pattern CaseSensitive :: MatchOption
pattern CaseSensitive = MatchOption' "CASE_SENSITIVE"

pattern Contains :: MatchOption
pattern Contains = MatchOption' "CONTAINS"

pattern EndsWith :: MatchOption
pattern EndsWith = MatchOption' "ENDS_WITH"

pattern Equals :: MatchOption
pattern Equals = MatchOption' "EQUALS"

pattern StartsWith :: MatchOption
pattern StartsWith = MatchOption' "STARTS_WITH"

{-# COMPLETE
  CaseInsensitive,
  CaseSensitive,
  Contains,
  EndsWith,
  Equals,
  StartsWith,
  MatchOption' #-}

instance FromText MatchOption where
    parser = (MatchOption' . mk) <$> takeText

instance ToText MatchOption where
    toText (MatchOption' ci) = original ci

-- | Represents an enum of /known/ $MatchOption.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MatchOption where
    toEnum i = case i of
        0 -> CaseInsensitive
        1 -> CaseSensitive
        2 -> Contains
        3 -> EndsWith
        4 -> Equals
        5 -> StartsWith
        _ -> (error . showText) $ "Unknown index for MatchOption: " <> toText i
    fromEnum x = case x of
        CaseInsensitive -> 0
        CaseSensitive -> 1
        Contains -> 2
        EndsWith -> 3
        Equals -> 4
        StartsWith -> 5
        MatchOption' name -> (error . showText) $ "Unknown MatchOption: " <> original name

-- | Represents the bounds of /known/ $MatchOption.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MatchOption where
    minBound = CaseInsensitive
    maxBound = StartsWith

instance Hashable     MatchOption
instance NFData       MatchOption
instance ToByteString MatchOption
instance ToQuery      MatchOption
instance ToHeader     MatchOption

instance ToJSON MatchOption where
    toJSON = toJSONText

instance FromJSON MatchOption where
    parseJSON = parseJSONText "MatchOption"
