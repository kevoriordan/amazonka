{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.PositionalConstraint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAF.Types.PositionalConstraint (
  PositionalConstraint (
    ..
    , Contains
    , ContainsWord
    , EndsWith
    , Exactly
    , StartsWith
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PositionalConstraint = PositionalConstraint' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern Contains :: PositionalConstraint
pattern Contains = PositionalConstraint' "CONTAINS"

pattern ContainsWord :: PositionalConstraint
pattern ContainsWord = PositionalConstraint' "CONTAINS_WORD"

pattern EndsWith :: PositionalConstraint
pattern EndsWith = PositionalConstraint' "ENDS_WITH"

pattern Exactly :: PositionalConstraint
pattern Exactly = PositionalConstraint' "EXACTLY"

pattern StartsWith :: PositionalConstraint
pattern StartsWith = PositionalConstraint' "STARTS_WITH"

{-# COMPLETE
  Contains,
  ContainsWord,
  EndsWith,
  Exactly,
  StartsWith,
  PositionalConstraint' #-}

instance FromText PositionalConstraint where
    parser = (PositionalConstraint' . mk) <$> takeText

instance ToText PositionalConstraint where
    toText (PositionalConstraint' ci) = original ci

-- | Represents an enum of /known/ $PositionalConstraint.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PositionalConstraint where
    toEnum i = case i of
        0 -> Contains
        1 -> ContainsWord
        2 -> EndsWith
        3 -> Exactly
        4 -> StartsWith
        _ -> (error . showText) $ "Unknown index for PositionalConstraint: " <> toText i
    fromEnum x = case x of
        Contains -> 0
        ContainsWord -> 1
        EndsWith -> 2
        Exactly -> 3
        StartsWith -> 4
        PositionalConstraint' name -> (error . showText) $ "Unknown PositionalConstraint: " <> original name

-- | Represents the bounds of /known/ $PositionalConstraint.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PositionalConstraint where
    minBound = Contains
    maxBound = StartsWith

instance Hashable     PositionalConstraint
instance NFData       PositionalConstraint
instance ToByteString PositionalConstraint
instance ToQuery      PositionalConstraint
instance ToHeader     PositionalConstraint

instance ToJSON PositionalConstraint where
    toJSON = toJSONText

instance FromJSON PositionalConstraint where
    parseJSON = parseJSONText "PositionalConstraint"
