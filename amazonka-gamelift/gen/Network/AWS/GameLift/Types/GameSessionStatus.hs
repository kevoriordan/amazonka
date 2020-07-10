{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameSessionStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.GameSessionStatus (
  GameSessionStatus (
    ..
    , GSSActivating
    , GSSActive
    , GSSError'
    , GSSTerminated
    , GSSTerminating
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data GameSessionStatus = GameSessionStatus' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern GSSActivating :: GameSessionStatus
pattern GSSActivating = GameSessionStatus' "ACTIVATING"

pattern GSSActive :: GameSessionStatus
pattern GSSActive = GameSessionStatus' "ACTIVE"

pattern GSSError' :: GameSessionStatus
pattern GSSError' = GameSessionStatus' "ERROR"

pattern GSSTerminated :: GameSessionStatus
pattern GSSTerminated = GameSessionStatus' "TERMINATED"

pattern GSSTerminating :: GameSessionStatus
pattern GSSTerminating = GameSessionStatus' "TERMINATING"

{-# COMPLETE
  GSSActivating,
  GSSActive,
  GSSError',
  GSSTerminated,
  GSSTerminating,
  GameSessionStatus' #-}

instance FromText GameSessionStatus where
    parser = (GameSessionStatus' . mk) <$> takeText

instance ToText GameSessionStatus where
    toText (GameSessionStatus' ci) = original ci

-- | Represents an enum of /known/ $GameSessionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum GameSessionStatus where
    toEnum i = case i of
        0 -> GSSActivating
        1 -> GSSActive
        2 -> GSSError'
        3 -> GSSTerminated
        4 -> GSSTerminating
        _ -> (error . showText) $ "Unknown index for GameSessionStatus: " <> toText i
    fromEnum x = case x of
        GSSActivating -> 0
        GSSActive -> 1
        GSSError' -> 2
        GSSTerminated -> 3
        GSSTerminating -> 4
        GameSessionStatus' name -> (error . showText) $ "Unknown GameSessionStatus: " <> original name

-- | Represents the bounds of /known/ $GameSessionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded GameSessionStatus where
    minBound = GSSActivating
    maxBound = GSSTerminating

instance Hashable     GameSessionStatus
instance NFData       GameSessionStatus
instance ToByteString GameSessionStatus
instance ToQuery      GameSessionStatus
instance ToHeader     GameSessionStatus

instance FromJSON GameSessionStatus where
    parseJSON = parseJSONText "GameSessionStatus"
