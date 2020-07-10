{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameServerGroupAction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.GameServerGroupAction (
  GameServerGroupAction (
    ..
    , ReplaceInstanceTypes
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data GameServerGroupAction = GameServerGroupAction' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern ReplaceInstanceTypes :: GameServerGroupAction
pattern ReplaceInstanceTypes = GameServerGroupAction' "REPLACE_INSTANCE_TYPES"

{-# COMPLETE
  ReplaceInstanceTypes,
  GameServerGroupAction' #-}

instance FromText GameServerGroupAction where
    parser = (GameServerGroupAction' . mk) <$> takeText

instance ToText GameServerGroupAction where
    toText (GameServerGroupAction' ci) = original ci

-- | Represents an enum of /known/ $GameServerGroupAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum GameServerGroupAction where
    toEnum i = case i of
        0 -> ReplaceInstanceTypes
        _ -> (error . showText) $ "Unknown index for GameServerGroupAction: " <> toText i
    fromEnum x = case x of
        ReplaceInstanceTypes -> 0
        GameServerGroupAction' name -> (error . showText) $ "Unknown GameServerGroupAction: " <> original name

-- | Represents the bounds of /known/ $GameServerGroupAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded GameServerGroupAction where
    minBound = ReplaceInstanceTypes
    maxBound = ReplaceInstanceTypes

instance Hashable     GameServerGroupAction
instance NFData       GameServerGroupAction
instance ToByteString GameServerGroupAction
instance ToQuery      GameServerGroupAction
instance ToHeader     GameServerGroupAction

instance ToJSON GameServerGroupAction where
    toJSON = toJSONText

instance FromJSON GameServerGroupAction where
    parseJSON = parseJSONText "GameServerGroupAction"
