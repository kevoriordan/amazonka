{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceDirectoryState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.WorkspaceDirectoryState (
  WorkspaceDirectoryState (
    ..
    , Deregistered
    , Deregistering
    , Error'
    , Registered
    , Registering
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data WorkspaceDirectoryState = WorkspaceDirectoryState' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern Deregistered :: WorkspaceDirectoryState
pattern Deregistered = WorkspaceDirectoryState' "DEREGISTERED"

pattern Deregistering :: WorkspaceDirectoryState
pattern Deregistering = WorkspaceDirectoryState' "DEREGISTERING"

pattern Error' :: WorkspaceDirectoryState
pattern Error' = WorkspaceDirectoryState' "ERROR"

pattern Registered :: WorkspaceDirectoryState
pattern Registered = WorkspaceDirectoryState' "REGISTERED"

pattern Registering :: WorkspaceDirectoryState
pattern Registering = WorkspaceDirectoryState' "REGISTERING"

{-# COMPLETE
  Deregistered,
  Deregistering,
  Error',
  Registered,
  Registering,
  WorkspaceDirectoryState' #-}

instance FromText WorkspaceDirectoryState where
    parser = (WorkspaceDirectoryState' . mk) <$> takeText

instance ToText WorkspaceDirectoryState where
    toText (WorkspaceDirectoryState' ci) = original ci

-- | Represents an enum of /known/ $WorkspaceDirectoryState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum WorkspaceDirectoryState where
    toEnum i = case i of
        0 -> Deregistered
        1 -> Deregistering
        2 -> Error'
        3 -> Registered
        4 -> Registering
        _ -> (error . showText) $ "Unknown index for WorkspaceDirectoryState: " <> toText i
    fromEnum x = case x of
        Deregistered -> 0
        Deregistering -> 1
        Error' -> 2
        Registered -> 3
        Registering -> 4
        WorkspaceDirectoryState' name -> (error . showText) $ "Unknown WorkspaceDirectoryState: " <> original name

-- | Represents the bounds of /known/ $WorkspaceDirectoryState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded WorkspaceDirectoryState where
    minBound = Deregistered
    maxBound = Registering

instance Hashable     WorkspaceDirectoryState
instance NFData       WorkspaceDirectoryState
instance ToByteString WorkspaceDirectoryState
instance ToQuery      WorkspaceDirectoryState
instance ToHeader     WorkspaceDirectoryState

instance FromJSON WorkspaceDirectoryState where
    parseJSON = parseJSONText "WorkspaceDirectoryState"
