{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceImageState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.WorkspaceImageState (
  WorkspaceImageState (
    ..
    , WISAvailable
    , WISError'
    , WISPending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data WorkspaceImageState = WorkspaceImageState' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern WISAvailable :: WorkspaceImageState
pattern WISAvailable = WorkspaceImageState' "AVAILABLE"

pattern WISError' :: WorkspaceImageState
pattern WISError' = WorkspaceImageState' "ERROR"

pattern WISPending :: WorkspaceImageState
pattern WISPending = WorkspaceImageState' "PENDING"

{-# COMPLETE
  WISAvailable,
  WISError',
  WISPending,
  WorkspaceImageState' #-}

instance FromText WorkspaceImageState where
    parser = (WorkspaceImageState' . mk) <$> takeText

instance ToText WorkspaceImageState where
    toText (WorkspaceImageState' ci) = original ci

-- | Represents an enum of /known/ $WorkspaceImageState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum WorkspaceImageState where
    toEnum i = case i of
        0 -> WISAvailable
        1 -> WISError'
        2 -> WISPending
        _ -> (error . showText) $ "Unknown index for WorkspaceImageState: " <> toText i
    fromEnum x = case x of
        WISAvailable -> 0
        WISError' -> 1
        WISPending -> 2
        WorkspaceImageState' name -> (error . showText) $ "Unknown WorkspaceImageState: " <> original name

-- | Represents the bounds of /known/ $WorkspaceImageState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded WorkspaceImageState where
    minBound = WISAvailable
    maxBound = WISPending

instance Hashable     WorkspaceImageState
instance NFData       WorkspaceImageState
instance ToByteString WorkspaceImageState
instance ToQuery      WorkspaceImageState
instance ToHeader     WorkspaceImageState

instance FromJSON WorkspaceImageState where
    parseJSON = parseJSONText "WorkspaceImageState"
