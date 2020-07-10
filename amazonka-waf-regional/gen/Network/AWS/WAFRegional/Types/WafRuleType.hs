{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.WafRuleType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAFRegional.Types.WafRuleType (
  WafRuleType (
    ..
    , Group
    , RateBased
    , Regular
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data WafRuleType = WafRuleType' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Group :: WafRuleType
pattern Group = WafRuleType' "GROUP"

pattern RateBased :: WafRuleType
pattern RateBased = WafRuleType' "RATE_BASED"

pattern Regular :: WafRuleType
pattern Regular = WafRuleType' "REGULAR"

{-# COMPLETE
  Group,
  RateBased,
  Regular,
  WafRuleType' #-}

instance FromText WafRuleType where
    parser = (WafRuleType' . mk) <$> takeText

instance ToText WafRuleType where
    toText (WafRuleType' ci) = original ci

-- | Represents an enum of /known/ $WafRuleType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum WafRuleType where
    toEnum i = case i of
        0 -> Group
        1 -> RateBased
        2 -> Regular
        _ -> (error . showText) $ "Unknown index for WafRuleType: " <> toText i
    fromEnum x = case x of
        Group -> 0
        RateBased -> 1
        Regular -> 2
        WafRuleType' name -> (error . showText) $ "Unknown WafRuleType: " <> original name

-- | Represents the bounds of /known/ $WafRuleType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded WafRuleType where
    minBound = Group
    maxBound = Regular

instance Hashable     WafRuleType
instance NFData       WafRuleType
instance ToByteString WafRuleType
instance ToQuery      WafRuleType
instance ToHeader     WafRuleType

instance ToJSON WafRuleType where
    toJSON = toJSONText

instance FromJSON WafRuleType where
    parseJSON = parseJSONText "WafRuleType"
