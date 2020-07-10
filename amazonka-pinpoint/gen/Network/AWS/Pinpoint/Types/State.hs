{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.State
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.State (
  State (
    ..
    , SActive
    , SCancelled
    , SClosed
    , SCompleted
    , SDraft
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data State = State' (CI Text)
               deriving (Eq, Ord, Read, Show, Data, Typeable,
                         Generic)

pattern SActive :: State
pattern SActive = State' "ACTIVE"

pattern SCancelled :: State
pattern SCancelled = State' "CANCELLED"

pattern SClosed :: State
pattern SClosed = State' "CLOSED"

pattern SCompleted :: State
pattern SCompleted = State' "COMPLETED"

pattern SDraft :: State
pattern SDraft = State' "DRAFT"

{-# COMPLETE
  SActive,
  SCancelled,
  SClosed,
  SCompleted,
  SDraft,
  State' #-}

instance FromText State where
    parser = (State' . mk) <$> takeText

instance ToText State where
    toText (State' ci) = original ci

-- | Represents an enum of /known/ $State.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum State where
    toEnum i = case i of
        0 -> SActive
        1 -> SCancelled
        2 -> SClosed
        3 -> SCompleted
        4 -> SDraft
        _ -> (error . showText) $ "Unknown index for State: " <> toText i
    fromEnum x = case x of
        SActive -> 0
        SCancelled -> 1
        SClosed -> 2
        SCompleted -> 3
        SDraft -> 4
        State' name -> (error . showText) $ "Unknown State: " <> original name

-- | Represents the bounds of /known/ $State.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded State where
    minBound = SActive
    maxBound = SDraft

instance Hashable     State
instance NFData       State
instance ToByteString State
instance ToQuery      State
instance ToHeader     State

instance ToJSON State where
    toJSON = toJSONText

instance FromJSON State where
    parseJSON = parseJSONText "State"
