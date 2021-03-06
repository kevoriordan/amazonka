{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.MappingRuleMatchType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentity.Types.MappingRuleMatchType (
  MappingRuleMatchType (
    ..
    , Contains
    , Equals
    , NotEqual
    , StartsWith
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MappingRuleMatchType = MappingRuleMatchType' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern Contains :: MappingRuleMatchType
pattern Contains = MappingRuleMatchType' "Contains"

pattern Equals :: MappingRuleMatchType
pattern Equals = MappingRuleMatchType' "Equals"

pattern NotEqual :: MappingRuleMatchType
pattern NotEqual = MappingRuleMatchType' "NotEqual"

pattern StartsWith :: MappingRuleMatchType
pattern StartsWith = MappingRuleMatchType' "StartsWith"

{-# COMPLETE
  Contains,
  Equals,
  NotEqual,
  StartsWith,
  MappingRuleMatchType' #-}

instance FromText MappingRuleMatchType where
    parser = (MappingRuleMatchType' . mk) <$> takeText

instance ToText MappingRuleMatchType where
    toText (MappingRuleMatchType' ci) = original ci

-- | Represents an enum of /known/ $MappingRuleMatchType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MappingRuleMatchType where
    toEnum i = case i of
        0 -> Contains
        1 -> Equals
        2 -> NotEqual
        3 -> StartsWith
        _ -> (error . showText) $ "Unknown index for MappingRuleMatchType: " <> toText i
    fromEnum x = case x of
        Contains -> 0
        Equals -> 1
        NotEqual -> 2
        StartsWith -> 3
        MappingRuleMatchType' name -> (error . showText) $ "Unknown MappingRuleMatchType: " <> original name

-- | Represents the bounds of /known/ $MappingRuleMatchType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MappingRuleMatchType where
    minBound = Contains
    maxBound = StartsWith

instance Hashable     MappingRuleMatchType
instance NFData       MappingRuleMatchType
instance ToByteString MappingRuleMatchType
instance ToQuery      MappingRuleMatchType
instance ToHeader     MappingRuleMatchType

instance ToJSON MappingRuleMatchType where
    toJSON = toJSONText

instance FromJSON MappingRuleMatchType where
    parseJSON = parseJSONText "MappingRuleMatchType"
