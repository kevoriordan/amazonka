{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameServerGroupDeleteOption
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.GameServerGroupDeleteOption (
  GameServerGroupDeleteOption (
    ..
    , ForceDelete
    , Retain
    , SafeDelete
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data GameServerGroupDeleteOption = GameServerGroupDeleteOption' (CI
                                                                   Text)
                                     deriving (Eq, Ord, Read, Show, Data,
                                               Typeable, Generic)

pattern ForceDelete :: GameServerGroupDeleteOption
pattern ForceDelete = GameServerGroupDeleteOption' "FORCE_DELETE"

pattern Retain :: GameServerGroupDeleteOption
pattern Retain = GameServerGroupDeleteOption' "RETAIN"

pattern SafeDelete :: GameServerGroupDeleteOption
pattern SafeDelete = GameServerGroupDeleteOption' "SAFE_DELETE"

{-# COMPLETE
  ForceDelete,
  Retain,
  SafeDelete,
  GameServerGroupDeleteOption' #-}

instance FromText GameServerGroupDeleteOption where
    parser = (GameServerGroupDeleteOption' . mk) <$> takeText

instance ToText GameServerGroupDeleteOption where
    toText (GameServerGroupDeleteOption' ci) = original ci

-- | Represents an enum of /known/ $GameServerGroupDeleteOption.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum GameServerGroupDeleteOption where
    toEnum i = case i of
        0 -> ForceDelete
        1 -> Retain
        2 -> SafeDelete
        _ -> (error . showText) $ "Unknown index for GameServerGroupDeleteOption: " <> toText i
    fromEnum x = case x of
        ForceDelete -> 0
        Retain -> 1
        SafeDelete -> 2
        GameServerGroupDeleteOption' name -> (error . showText) $ "Unknown GameServerGroupDeleteOption: " <> original name

-- | Represents the bounds of /known/ $GameServerGroupDeleteOption.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded GameServerGroupDeleteOption where
    minBound = ForceDelete
    maxBound = SafeDelete

instance Hashable     GameServerGroupDeleteOption
instance NFData       GameServerGroupDeleteOption
instance ToByteString GameServerGroupDeleteOption
instance ToQuery      GameServerGroupDeleteOption
instance ToHeader     GameServerGroupDeleteOption

instance ToJSON GameServerGroupDeleteOption where
    toJSON = toJSONText
