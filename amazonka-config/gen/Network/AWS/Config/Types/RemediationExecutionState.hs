{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RemediationExecutionState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.RemediationExecutionState (
  RemediationExecutionState (
    ..
    , RESFailed
    , RESInProgress
    , RESQueued
    , RESSucceeded
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RemediationExecutionState = RemediationExecutionState' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern RESFailed :: RemediationExecutionState
pattern RESFailed = RemediationExecutionState' "FAILED"

pattern RESInProgress :: RemediationExecutionState
pattern RESInProgress = RemediationExecutionState' "IN_PROGRESS"

pattern RESQueued :: RemediationExecutionState
pattern RESQueued = RemediationExecutionState' "QUEUED"

pattern RESSucceeded :: RemediationExecutionState
pattern RESSucceeded = RemediationExecutionState' "SUCCEEDED"

{-# COMPLETE
  RESFailed,
  RESInProgress,
  RESQueued,
  RESSucceeded,
  RemediationExecutionState' #-}

instance FromText RemediationExecutionState where
    parser = (RemediationExecutionState' . mk) <$> takeText

instance ToText RemediationExecutionState where
    toText (RemediationExecutionState' ci) = original ci

-- | Represents an enum of /known/ $RemediationExecutionState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RemediationExecutionState where
    toEnum i = case i of
        0 -> RESFailed
        1 -> RESInProgress
        2 -> RESQueued
        3 -> RESSucceeded
        _ -> (error . showText) $ "Unknown index for RemediationExecutionState: " <> toText i
    fromEnum x = case x of
        RESFailed -> 0
        RESInProgress -> 1
        RESQueued -> 2
        RESSucceeded -> 3
        RemediationExecutionState' name -> (error . showText) $ "Unknown RemediationExecutionState: " <> original name

-- | Represents the bounds of /known/ $RemediationExecutionState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RemediationExecutionState where
    minBound = RESFailed
    maxBound = RESSucceeded

instance Hashable     RemediationExecutionState
instance NFData       RemediationExecutionState
instance ToByteString RemediationExecutionState
instance ToQuery      RemediationExecutionState
instance ToHeader     RemediationExecutionState

instance FromJSON RemediationExecutionState where
    parseJSON = parseJSONText "RemediationExecutionState"
