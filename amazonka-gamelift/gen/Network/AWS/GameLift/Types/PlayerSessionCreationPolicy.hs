{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.PlayerSessionCreationPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.PlayerSessionCreationPolicy (
  PlayerSessionCreationPolicy (
    ..
    , AcceptAll
    , DenyAll
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PlayerSessionCreationPolicy = PlayerSessionCreationPolicy' (CI
                                                                   Text)
                                     deriving (Eq, Ord, Read, Show, Data,
                                               Typeable, Generic)

pattern AcceptAll :: PlayerSessionCreationPolicy
pattern AcceptAll = PlayerSessionCreationPolicy' "ACCEPT_ALL"

pattern DenyAll :: PlayerSessionCreationPolicy
pattern DenyAll = PlayerSessionCreationPolicy' "DENY_ALL"

{-# COMPLETE
  AcceptAll,
  DenyAll,
  PlayerSessionCreationPolicy' #-}

instance FromText PlayerSessionCreationPolicy where
    parser = (PlayerSessionCreationPolicy' . mk) <$> takeText

instance ToText PlayerSessionCreationPolicy where
    toText (PlayerSessionCreationPolicy' ci) = original ci

-- | Represents an enum of /known/ $PlayerSessionCreationPolicy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PlayerSessionCreationPolicy where
    toEnum i = case i of
        0 -> AcceptAll
        1 -> DenyAll
        _ -> (error . showText) $ "Unknown index for PlayerSessionCreationPolicy: " <> toText i
    fromEnum x = case x of
        AcceptAll -> 0
        DenyAll -> 1
        PlayerSessionCreationPolicy' name -> (error . showText) $ "Unknown PlayerSessionCreationPolicy: " <> original name

-- | Represents the bounds of /known/ $PlayerSessionCreationPolicy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PlayerSessionCreationPolicy where
    minBound = AcceptAll
    maxBound = DenyAll

instance Hashable     PlayerSessionCreationPolicy
instance NFData       PlayerSessionCreationPolicy
instance ToByteString PlayerSessionCreationPolicy
instance ToQuery      PlayerSessionCreationPolicy
instance ToHeader     PlayerSessionCreationPolicy

instance ToJSON PlayerSessionCreationPolicy where
    toJSON = toJSONText

instance FromJSON PlayerSessionCreationPolicy where
    parseJSON = parseJSONText "PlayerSessionCreationPolicy"
