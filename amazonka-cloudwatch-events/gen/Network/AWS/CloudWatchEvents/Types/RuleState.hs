{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.RuleState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchEvents.Types.RuleState (
  RuleState (
    ..
    , Disabled
    , Enabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RuleState = RuleState' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern Disabled :: RuleState
pattern Disabled = RuleState' "DISABLED"

pattern Enabled :: RuleState
pattern Enabled = RuleState' "ENABLED"

{-# COMPLETE
  Disabled,
  Enabled,
  RuleState' #-}

instance FromText RuleState where
    parser = (RuleState' . mk) <$> takeText

instance ToText RuleState where
    toText (RuleState' ci) = original ci

-- | Represents an enum of /known/ $RuleState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RuleState where
    toEnum i = case i of
        0 -> Disabled
        1 -> Enabled
        _ -> (error . showText) $ "Unknown index for RuleState: " <> toText i
    fromEnum x = case x of
        Disabled -> 0
        Enabled -> 1
        RuleState' name -> (error . showText) $ "Unknown RuleState: " <> original name

-- | Represents the bounds of /known/ $RuleState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RuleState where
    minBound = Disabled
    maxBound = Enabled

instance Hashable     RuleState
instance NFData       RuleState
instance ToByteString RuleState
instance ToQuery      RuleState
instance ToHeader     RuleState

instance ToJSON RuleState where
    toJSON = toJSONText

instance FromJSON RuleState where
    parseJSON = parseJSONText "RuleState"
