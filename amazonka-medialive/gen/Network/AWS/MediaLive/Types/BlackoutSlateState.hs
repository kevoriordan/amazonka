{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BlackoutSlateState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.BlackoutSlateState (
  BlackoutSlateState (
    ..
    , BSSDisabled
    , BSSEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Blackout Slate State
data BlackoutSlateState = BlackoutSlateState' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern BSSDisabled :: BlackoutSlateState
pattern BSSDisabled = BlackoutSlateState' "DISABLED"

pattern BSSEnabled :: BlackoutSlateState
pattern BSSEnabled = BlackoutSlateState' "ENABLED"

{-# COMPLETE
  BSSDisabled,
  BSSEnabled,
  BlackoutSlateState' #-}

instance FromText BlackoutSlateState where
    parser = (BlackoutSlateState' . mk) <$> takeText

instance ToText BlackoutSlateState where
    toText (BlackoutSlateState' ci) = original ci

-- | Represents an enum of /known/ $BlackoutSlateState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BlackoutSlateState where
    toEnum i = case i of
        0 -> BSSDisabled
        1 -> BSSEnabled
        _ -> (error . showText) $ "Unknown index for BlackoutSlateState: " <> toText i
    fromEnum x = case x of
        BSSDisabled -> 0
        BSSEnabled -> 1
        BlackoutSlateState' name -> (error . showText) $ "Unknown BlackoutSlateState: " <> original name

-- | Represents the bounds of /known/ $BlackoutSlateState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BlackoutSlateState where
    minBound = BSSDisabled
    maxBound = BSSEnabled

instance Hashable     BlackoutSlateState
instance NFData       BlackoutSlateState
instance ToByteString BlackoutSlateState
instance ToQuery      BlackoutSlateState
instance ToHeader     BlackoutSlateState

instance ToJSON BlackoutSlateState where
    toJSON = toJSONText

instance FromJSON BlackoutSlateState where
    parseJSON = parseJSONText "BlackoutSlateState"
