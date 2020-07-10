{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigRuleState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.ConfigRuleState (
  ConfigRuleState (
    ..
    , Active
    , Deleting
    , DeletingResults
    , Evaluating
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ConfigRuleState = ConfigRuleState' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Active :: ConfigRuleState
pattern Active = ConfigRuleState' "ACTIVE"

pattern Deleting :: ConfigRuleState
pattern Deleting = ConfigRuleState' "DELETING"

pattern DeletingResults :: ConfigRuleState
pattern DeletingResults = ConfigRuleState' "DELETING_RESULTS"

pattern Evaluating :: ConfigRuleState
pattern Evaluating = ConfigRuleState' "EVALUATING"

{-# COMPLETE
  Active,
  Deleting,
  DeletingResults,
  Evaluating,
  ConfigRuleState' #-}

instance FromText ConfigRuleState where
    parser = (ConfigRuleState' . mk) <$> takeText

instance ToText ConfigRuleState where
    toText (ConfigRuleState' ci) = original ci

-- | Represents an enum of /known/ $ConfigRuleState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ConfigRuleState where
    toEnum i = case i of
        0 -> Active
        1 -> Deleting
        2 -> DeletingResults
        3 -> Evaluating
        _ -> (error . showText) $ "Unknown index for ConfigRuleState: " <> toText i
    fromEnum x = case x of
        Active -> 0
        Deleting -> 1
        DeletingResults -> 2
        Evaluating -> 3
        ConfigRuleState' name -> (error . showText) $ "Unknown ConfigRuleState: " <> original name

-- | Represents the bounds of /known/ $ConfigRuleState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ConfigRuleState where
    minBound = Active
    maxBound = Evaluating

instance Hashable     ConfigRuleState
instance NFData       ConfigRuleState
instance ToByteString ConfigRuleState
instance ToQuery      ConfigRuleState
instance ToHeader     ConfigRuleState

instance ToJSON ConfigRuleState where
    toJSON = toJSONText

instance FromJSON ConfigRuleState where
    parseJSON = parseJSONText "ConfigRuleState"
