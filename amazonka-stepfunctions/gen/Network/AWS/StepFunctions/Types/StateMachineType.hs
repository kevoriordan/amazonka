{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.StateMachineType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.StateMachineType (
  StateMachineType (
    ..
    , Express
    , Standard
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StateMachineType = StateMachineType' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern Express :: StateMachineType
pattern Express = StateMachineType' "EXPRESS"

pattern Standard :: StateMachineType
pattern Standard = StateMachineType' "STANDARD"

{-# COMPLETE
  Express,
  Standard,
  StateMachineType' #-}

instance FromText StateMachineType where
    parser = (StateMachineType' . mk) <$> takeText

instance ToText StateMachineType where
    toText (StateMachineType' ci) = original ci

-- | Represents an enum of /known/ $StateMachineType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StateMachineType where
    toEnum i = case i of
        0 -> Express
        1 -> Standard
        _ -> (error . showText) $ "Unknown index for StateMachineType: " <> toText i
    fromEnum x = case x of
        Express -> 0
        Standard -> 1
        StateMachineType' name -> (error . showText) $ "Unknown StateMachineType: " <> original name

-- | Represents the bounds of /known/ $StateMachineType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StateMachineType where
    minBound = Express
    maxBound = Standard

instance Hashable     StateMachineType
instance NFData       StateMachineType
instance ToByteString StateMachineType
instance ToQuery      StateMachineType
instance ToHeader     StateMachineType

instance ToJSON StateMachineType where
    toJSON = toJSONText

instance FromJSON StateMachineType where
    parseJSON = parseJSONText "StateMachineType"
