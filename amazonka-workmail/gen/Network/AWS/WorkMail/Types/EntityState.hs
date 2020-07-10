{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.EntityState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkMail.Types.EntityState (
  EntityState (
    ..
    , Deleted
    , Disabled
    , Enabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EntityState = EntityState' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Deleted :: EntityState
pattern Deleted = EntityState' "DELETED"

pattern Disabled :: EntityState
pattern Disabled = EntityState' "DISABLED"

pattern Enabled :: EntityState
pattern Enabled = EntityState' "ENABLED"

{-# COMPLETE
  Deleted,
  Disabled,
  Enabled,
  EntityState' #-}

instance FromText EntityState where
    parser = (EntityState' . mk) <$> takeText

instance ToText EntityState where
    toText (EntityState' ci) = original ci

-- | Represents an enum of /known/ $EntityState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EntityState where
    toEnum i = case i of
        0 -> Deleted
        1 -> Disabled
        2 -> Enabled
        _ -> (error . showText) $ "Unknown index for EntityState: " <> toText i
    fromEnum x = case x of
        Deleted -> 0
        Disabled -> 1
        Enabled -> 2
        EntityState' name -> (error . showText) $ "Unknown EntityState: " <> original name

-- | Represents the bounds of /known/ $EntityState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EntityState where
    minBound = Deleted
    maxBound = Enabled

instance Hashable     EntityState
instance NFData       EntityState
instance ToByteString EntityState
instance ToQuery      EntityState
instance ToHeader     EntityState

instance FromJSON EntityState where
    parseJSON = parseJSONText "EntityState"
