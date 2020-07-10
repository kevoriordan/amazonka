{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.KeyState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KMS.Types.KeyState (
  KeyState (
    ..
    , Disabled
    , Enabled
    , PendingDeletion
    , PendingImport
    , Unavailable
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data KeyState = KeyState' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern Disabled :: KeyState
pattern Disabled = KeyState' "Disabled"

pattern Enabled :: KeyState
pattern Enabled = KeyState' "Enabled"

pattern PendingDeletion :: KeyState
pattern PendingDeletion = KeyState' "PendingDeletion"

pattern PendingImport :: KeyState
pattern PendingImport = KeyState' "PendingImport"

pattern Unavailable :: KeyState
pattern Unavailable = KeyState' "Unavailable"

{-# COMPLETE
  Disabled,
  Enabled,
  PendingDeletion,
  PendingImport,
  Unavailable,
  KeyState' #-}

instance FromText KeyState where
    parser = (KeyState' . mk) <$> takeText

instance ToText KeyState where
    toText (KeyState' ci) = original ci

-- | Represents an enum of /known/ $KeyState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum KeyState where
    toEnum i = case i of
        0 -> Disabled
        1 -> Enabled
        2 -> PendingDeletion
        3 -> PendingImport
        4 -> Unavailable
        _ -> (error . showText) $ "Unknown index for KeyState: " <> toText i
    fromEnum x = case x of
        Disabled -> 0
        Enabled -> 1
        PendingDeletion -> 2
        PendingImport -> 3
        Unavailable -> 4
        KeyState' name -> (error . showText) $ "Unknown KeyState: " <> original name

-- | Represents the bounds of /known/ $KeyState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded KeyState where
    minBound = Disabled
    maxBound = Unavailable

instance Hashable     KeyState
instance NFData       KeyState
instance ToByteString KeyState
instance ToQuery      KeyState
instance ToHeader     KeyState

instance FromJSON KeyState where
    parseJSON = parseJSONText "KeyState"
