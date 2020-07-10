{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.BGPStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.BGPStatus (
  BGPStatus (
    ..
    , Down
    , UP
    , Unknown
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data BGPStatus = BGPStatus' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern Down :: BGPStatus
pattern Down = BGPStatus' "down"

pattern UP :: BGPStatus
pattern UP = BGPStatus' "up"

pattern Unknown :: BGPStatus
pattern Unknown = BGPStatus' "unknown"

{-# COMPLETE
  Down,
  UP,
  Unknown,
  BGPStatus' #-}

instance FromText BGPStatus where
    parser = (BGPStatus' . mk) <$> takeText

instance ToText BGPStatus where
    toText (BGPStatus' ci) = original ci

-- | Represents an enum of /known/ $BGPStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BGPStatus where
    toEnum i = case i of
        0 -> Down
        1 -> UP
        2 -> Unknown
        _ -> (error . showText) $ "Unknown index for BGPStatus: " <> toText i
    fromEnum x = case x of
        Down -> 0
        UP -> 1
        Unknown -> 2
        BGPStatus' name -> (error . showText) $ "Unknown BGPStatus: " <> original name

-- | Represents the bounds of /known/ $BGPStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BGPStatus where
    minBound = Down
    maxBound = Unknown

instance Hashable     BGPStatus
instance NFData       BGPStatus
instance ToByteString BGPStatus
instance ToQuery      BGPStatus
instance ToHeader     BGPStatus

instance FromJSON BGPStatus where
    parseJSON = parseJSONText "BGPStatus"
