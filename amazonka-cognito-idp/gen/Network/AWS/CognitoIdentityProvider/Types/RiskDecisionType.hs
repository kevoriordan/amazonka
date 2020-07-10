{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.RiskDecisionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.RiskDecisionType (
  RiskDecisionType (
    ..
    , AccountTakeover
    , Block
    , NoRisk
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RiskDecisionType = RiskDecisionType' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern AccountTakeover :: RiskDecisionType
pattern AccountTakeover = RiskDecisionType' "AccountTakeover"

pattern Block :: RiskDecisionType
pattern Block = RiskDecisionType' "Block"

pattern NoRisk :: RiskDecisionType
pattern NoRisk = RiskDecisionType' "NoRisk"

{-# COMPLETE
  AccountTakeover,
  Block,
  NoRisk,
  RiskDecisionType' #-}

instance FromText RiskDecisionType where
    parser = (RiskDecisionType' . mk) <$> takeText

instance ToText RiskDecisionType where
    toText (RiskDecisionType' ci) = original ci

-- | Represents an enum of /known/ $RiskDecisionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RiskDecisionType where
    toEnum i = case i of
        0 -> AccountTakeover
        1 -> Block
        2 -> NoRisk
        _ -> (error . showText) $ "Unknown index for RiskDecisionType: " <> toText i
    fromEnum x = case x of
        AccountTakeover -> 0
        Block -> 1
        NoRisk -> 2
        RiskDecisionType' name -> (error . showText) $ "Unknown RiskDecisionType: " <> original name

-- | Represents the bounds of /known/ $RiskDecisionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RiskDecisionType where
    minBound = AccountTakeover
    maxBound = NoRisk

instance Hashable     RiskDecisionType
instance NFData       RiskDecisionType
instance ToByteString RiskDecisionType
instance ToQuery      RiskDecisionType
instance ToHeader     RiskDecisionType

instance FromJSON RiskDecisionType where
    parseJSON = parseJSONText "RiskDecisionType"
