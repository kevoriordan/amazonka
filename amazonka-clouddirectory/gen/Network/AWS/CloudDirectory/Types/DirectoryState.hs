{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.DirectoryState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.DirectoryState (
  DirectoryState (
    ..
    , Deleted
    , Disabled
    , Enabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DirectoryState = DirectoryState' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern Deleted :: DirectoryState
pattern Deleted = DirectoryState' "DELETED"

pattern Disabled :: DirectoryState
pattern Disabled = DirectoryState' "DISABLED"

pattern Enabled :: DirectoryState
pattern Enabled = DirectoryState' "ENABLED"

{-# COMPLETE
  Deleted,
  Disabled,
  Enabled,
  DirectoryState' #-}

instance FromText DirectoryState where
    parser = (DirectoryState' . mk) <$> takeText

instance ToText DirectoryState where
    toText (DirectoryState' ci) = original ci

-- | Represents an enum of /known/ $DirectoryState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DirectoryState where
    toEnum i = case i of
        0 -> Deleted
        1 -> Disabled
        2 -> Enabled
        _ -> (error . showText) $ "Unknown index for DirectoryState: " <> toText i
    fromEnum x = case x of
        Deleted -> 0
        Disabled -> 1
        Enabled -> 2
        DirectoryState' name -> (error . showText) $ "Unknown DirectoryState: " <> original name

-- | Represents the bounds of /known/ $DirectoryState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DirectoryState where
    minBound = Deleted
    maxBound = Enabled

instance Hashable     DirectoryState
instance NFData       DirectoryState
instance ToByteString DirectoryState
instance ToQuery      DirectoryState
instance ToHeader     DirectoryState

instance ToJSON DirectoryState where
    toJSON = toJSONText

instance FromJSON DirectoryState where
    parseJSON = parseJSONText "DirectoryState"
