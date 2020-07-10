{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameSessionStatusReason
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.GameSessionStatusReason (
  GameSessionStatusReason (
    ..
    , Interrupted
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data GameSessionStatusReason = GameSessionStatusReason' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern Interrupted :: GameSessionStatusReason
pattern Interrupted = GameSessionStatusReason' "INTERRUPTED"

{-# COMPLETE
  Interrupted,
  GameSessionStatusReason' #-}

instance FromText GameSessionStatusReason where
    parser = (GameSessionStatusReason' . mk) <$> takeText

instance ToText GameSessionStatusReason where
    toText (GameSessionStatusReason' ci) = original ci

-- | Represents an enum of /known/ $GameSessionStatusReason.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum GameSessionStatusReason where
    toEnum i = case i of
        0 -> Interrupted
        _ -> (error . showText) $ "Unknown index for GameSessionStatusReason: " <> toText i
    fromEnum x = case x of
        Interrupted -> 0
        GameSessionStatusReason' name -> (error . showText) $ "Unknown GameSessionStatusReason: " <> original name

-- | Represents the bounds of /known/ $GameSessionStatusReason.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded GameSessionStatusReason where
    minBound = Interrupted
    maxBound = Interrupted

instance Hashable     GameSessionStatusReason
instance NFData       GameSessionStatusReason
instance ToByteString GameSessionStatusReason
instance ToQuery      GameSessionStatusReason
instance ToHeader     GameSessionStatusReason

instance FromJSON GameSessionStatusReason where
    parseJSON = parseJSONText "GameSessionStatusReason"
