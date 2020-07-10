{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.LifeCycleState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EFS.Types.LifeCycleState (
  LifeCycleState (
    ..
    , Available
    , Creating
    , Deleted
    , Deleting
    , Updating
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LifeCycleState = LifeCycleState' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern Available :: LifeCycleState
pattern Available = LifeCycleState' "available"

pattern Creating :: LifeCycleState
pattern Creating = LifeCycleState' "creating"

pattern Deleted :: LifeCycleState
pattern Deleted = LifeCycleState' "deleted"

pattern Deleting :: LifeCycleState
pattern Deleting = LifeCycleState' "deleting"

pattern Updating :: LifeCycleState
pattern Updating = LifeCycleState' "updating"

{-# COMPLETE
  Available,
  Creating,
  Deleted,
  Deleting,
  Updating,
  LifeCycleState' #-}

instance FromText LifeCycleState where
    parser = (LifeCycleState' . mk) <$> takeText

instance ToText LifeCycleState where
    toText (LifeCycleState' ci) = original ci

-- | Represents an enum of /known/ $LifeCycleState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LifeCycleState where
    toEnum i = case i of
        0 -> Available
        1 -> Creating
        2 -> Deleted
        3 -> Deleting
        4 -> Updating
        _ -> (error . showText) $ "Unknown index for LifeCycleState: " <> toText i
    fromEnum x = case x of
        Available -> 0
        Creating -> 1
        Deleted -> 2
        Deleting -> 3
        Updating -> 4
        LifeCycleState' name -> (error . showText) $ "Unknown LifeCycleState: " <> original name

-- | Represents the bounds of /known/ $LifeCycleState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LifeCycleState where
    minBound = Available
    maxBound = Updating

instance Hashable     LifeCycleState
instance NFData       LifeCycleState
instance ToByteString LifeCycleState
instance ToQuery      LifeCycleState
instance ToHeader     LifeCycleState

instance FromJSON LifeCycleState where
    parseJSON = parseJSONText "LifeCycleState"
