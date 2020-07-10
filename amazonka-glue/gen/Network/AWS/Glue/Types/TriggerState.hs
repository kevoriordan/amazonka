{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TriggerState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.TriggerState (
  TriggerState (
    ..
    , TSActivated
    , TSActivating
    , TSCreated
    , TSCreating
    , TSDeactivated
    , TSDeactivating
    , TSDeleting
    , TSUpdating
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TriggerState = TriggerState' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern TSActivated :: TriggerState
pattern TSActivated = TriggerState' "ACTIVATED"

pattern TSActivating :: TriggerState
pattern TSActivating = TriggerState' "ACTIVATING"

pattern TSCreated :: TriggerState
pattern TSCreated = TriggerState' "CREATED"

pattern TSCreating :: TriggerState
pattern TSCreating = TriggerState' "CREATING"

pattern TSDeactivated :: TriggerState
pattern TSDeactivated = TriggerState' "DEACTIVATED"

pattern TSDeactivating :: TriggerState
pattern TSDeactivating = TriggerState' "DEACTIVATING"

pattern TSDeleting :: TriggerState
pattern TSDeleting = TriggerState' "DELETING"

pattern TSUpdating :: TriggerState
pattern TSUpdating = TriggerState' "UPDATING"

{-# COMPLETE
  TSActivated,
  TSActivating,
  TSCreated,
  TSCreating,
  TSDeactivated,
  TSDeactivating,
  TSDeleting,
  TSUpdating,
  TriggerState' #-}

instance FromText TriggerState where
    parser = (TriggerState' . mk) <$> takeText

instance ToText TriggerState where
    toText (TriggerState' ci) = original ci

-- | Represents an enum of /known/ $TriggerState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TriggerState where
    toEnum i = case i of
        0 -> TSActivated
        1 -> TSActivating
        2 -> TSCreated
        3 -> TSCreating
        4 -> TSDeactivated
        5 -> TSDeactivating
        6 -> TSDeleting
        7 -> TSUpdating
        _ -> (error . showText) $ "Unknown index for TriggerState: " <> toText i
    fromEnum x = case x of
        TSActivated -> 0
        TSActivating -> 1
        TSCreated -> 2
        TSCreating -> 3
        TSDeactivated -> 4
        TSDeactivating -> 5
        TSDeleting -> 6
        TSUpdating -> 7
        TriggerState' name -> (error . showText) $ "Unknown TriggerState: " <> original name

-- | Represents the bounds of /known/ $TriggerState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TriggerState where
    minBound = TSActivated
    maxBound = TSUpdating

instance Hashable     TriggerState
instance NFData       TriggerState
instance ToByteString TriggerState
instance ToQuery      TriggerState
instance ToHeader     TriggerState

instance FromJSON TriggerState where
    parseJSON = parseJSONText "TriggerState"
