{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.Context
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.Context (
  Context (
    ..
    , CostAndUsage
    , Reservations
    , SavingsPlans
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Context = Context' (CI Text)
                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                           Generic)

pattern CostAndUsage :: Context
pattern CostAndUsage = Context' "COST_AND_USAGE"

pattern Reservations :: Context
pattern Reservations = Context' "RESERVATIONS"

pattern SavingsPlans :: Context
pattern SavingsPlans = Context' "SAVINGS_PLANS"

{-# COMPLETE
  CostAndUsage,
  Reservations,
  SavingsPlans,
  Context' #-}

instance FromText Context where
    parser = (Context' . mk) <$> takeText

instance ToText Context where
    toText (Context' ci) = original ci

-- | Represents an enum of /known/ $Context.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Context where
    toEnum i = case i of
        0 -> CostAndUsage
        1 -> Reservations
        2 -> SavingsPlans
        _ -> (error . showText) $ "Unknown index for Context: " <> toText i
    fromEnum x = case x of
        CostAndUsage -> 0
        Reservations -> 1
        SavingsPlans -> 2
        Context' name -> (error . showText) $ "Unknown Context: " <> original name

-- | Represents the bounds of /known/ $Context.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Context where
    minBound = CostAndUsage
    maxBound = SavingsPlans

instance Hashable     Context
instance NFData       Context
instance ToByteString Context
instance ToQuery      Context
instance ToHeader     Context

instance ToJSON Context where
    toJSON = toJSONText
