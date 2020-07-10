{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Scope
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.Scope (
  Scope (
    ..
    , Shared
    , Task
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Scope = Scope' (CI Text)
               deriving (Eq, Ord, Read, Show, Data, Typeable,
                         Generic)

pattern Shared :: Scope
pattern Shared = Scope' "shared"

pattern Task :: Scope
pattern Task = Scope' "task"

{-# COMPLETE
  Shared,
  Task,
  Scope' #-}

instance FromText Scope where
    parser = (Scope' . mk) <$> takeText

instance ToText Scope where
    toText (Scope' ci) = original ci

-- | Represents an enum of /known/ $Scope.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Scope where
    toEnum i = case i of
        0 -> Shared
        1 -> Task
        _ -> (error . showText) $ "Unknown index for Scope: " <> toText i
    fromEnum x = case x of
        Shared -> 0
        Task -> 1
        Scope' name -> (error . showText) $ "Unknown Scope: " <> original name

-- | Represents the bounds of /known/ $Scope.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Scope where
    minBound = Shared
    maxBound = Task

instance Hashable     Scope
instance NFData       Scope
instance ToByteString Scope
instance ToQuery      Scope
instance ToHeader     Scope

instance ToJSON Scope where
    toJSON = toJSONText

instance FromJSON Scope where
    parseJSON = parseJSONText "Scope"
