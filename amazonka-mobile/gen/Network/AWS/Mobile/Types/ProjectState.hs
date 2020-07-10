{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.Types.ProjectState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Mobile.Types.ProjectState (
  ProjectState (
    ..
    , Importing
    , Normal
    , Syncing
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Synchronization state for a project. 
--
--
data ProjectState = ProjectState' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Importing :: ProjectState
pattern Importing = ProjectState' "IMPORTING"

pattern Normal :: ProjectState
pattern Normal = ProjectState' "NORMAL"

pattern Syncing :: ProjectState
pattern Syncing = ProjectState' "SYNCING"

{-# COMPLETE
  Importing,
  Normal,
  Syncing,
  ProjectState' #-}

instance FromText ProjectState where
    parser = (ProjectState' . mk) <$> takeText

instance ToText ProjectState where
    toText (ProjectState' ci) = original ci

-- | Represents an enum of /known/ $ProjectState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProjectState where
    toEnum i = case i of
        0 -> Importing
        1 -> Normal
        2 -> Syncing
        _ -> (error . showText) $ "Unknown index for ProjectState: " <> toText i
    fromEnum x = case x of
        Importing -> 0
        Normal -> 1
        Syncing -> 2
        ProjectState' name -> (error . showText) $ "Unknown ProjectState: " <> original name

-- | Represents the bounds of /known/ $ProjectState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProjectState where
    minBound = Importing
    maxBound = Syncing

instance Hashable     ProjectState
instance NFData       ProjectState
instance ToByteString ProjectState
instance ToQuery      ProjectState
instance ToHeader     ProjectState

instance FromJSON ProjectState where
    parseJSON = parseJSONText "ProjectState"
