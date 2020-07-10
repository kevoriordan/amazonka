{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConformancePackState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.ConformancePackState (
  ConformancePackState (
    ..
    , CPSCreateComplete
    , CPSCreateFailed
    , CPSCreateInProgress
    , CPSDeleteFailed
    , CPSDeleteInProgress
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ConformancePackState = ConformancePackState' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern CPSCreateComplete :: ConformancePackState
pattern CPSCreateComplete = ConformancePackState' "CREATE_COMPLETE"

pattern CPSCreateFailed :: ConformancePackState
pattern CPSCreateFailed = ConformancePackState' "CREATE_FAILED"

pattern CPSCreateInProgress :: ConformancePackState
pattern CPSCreateInProgress = ConformancePackState' "CREATE_IN_PROGRESS"

pattern CPSDeleteFailed :: ConformancePackState
pattern CPSDeleteFailed = ConformancePackState' "DELETE_FAILED"

pattern CPSDeleteInProgress :: ConformancePackState
pattern CPSDeleteInProgress = ConformancePackState' "DELETE_IN_PROGRESS"

{-# COMPLETE
  CPSCreateComplete,
  CPSCreateFailed,
  CPSCreateInProgress,
  CPSDeleteFailed,
  CPSDeleteInProgress,
  ConformancePackState' #-}

instance FromText ConformancePackState where
    parser = (ConformancePackState' . mk) <$> takeText

instance ToText ConformancePackState where
    toText (ConformancePackState' ci) = original ci

-- | Represents an enum of /known/ $ConformancePackState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ConformancePackState where
    toEnum i = case i of
        0 -> CPSCreateComplete
        1 -> CPSCreateFailed
        2 -> CPSCreateInProgress
        3 -> CPSDeleteFailed
        4 -> CPSDeleteInProgress
        _ -> (error . showText) $ "Unknown index for ConformancePackState: " <> toText i
    fromEnum x = case x of
        CPSCreateComplete -> 0
        CPSCreateFailed -> 1
        CPSCreateInProgress -> 2
        CPSDeleteFailed -> 3
        CPSDeleteInProgress -> 4
        ConformancePackState' name -> (error . showText) $ "Unknown ConformancePackState: " <> original name

-- | Represents the bounds of /known/ $ConformancePackState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ConformancePackState where
    minBound = CPSCreateComplete
    maxBound = CPSDeleteInProgress

instance Hashable     ConformancePackState
instance NFData       ConformancePackState
instance ToByteString ConformancePackState
instance ToQuery      ConformancePackState
instance ToHeader     ConformancePackState

instance FromJSON ConformancePackState where
    parseJSON = parseJSONText "ConformancePackState"
