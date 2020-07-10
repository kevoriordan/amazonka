{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.PortState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.PortState (
  PortState (
    ..
    , Closed
    , Open
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PortState = PortState' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern Closed :: PortState
pattern Closed = PortState' "closed"

pattern Open :: PortState
pattern Open = PortState' "open"

{-# COMPLETE
  Closed,
  Open,
  PortState' #-}

instance FromText PortState where
    parser = (PortState' . mk) <$> takeText

instance ToText PortState where
    toText (PortState' ci) = original ci

-- | Represents an enum of /known/ $PortState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PortState where
    toEnum i = case i of
        0 -> Closed
        1 -> Open
        _ -> (error . showText) $ "Unknown index for PortState: " <> toText i
    fromEnum x = case x of
        Closed -> 0
        Open -> 1
        PortState' name -> (error . showText) $ "Unknown PortState: " <> original name

-- | Represents the bounds of /known/ $PortState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PortState where
    minBound = Closed
    maxBound = Open

instance Hashable     PortState
instance NFData       PortState
instance ToByteString PortState
instance ToQuery      PortState
instance ToHeader     PortState

instance FromJSON PortState where
    parseJSON = parseJSONText "PortState"
