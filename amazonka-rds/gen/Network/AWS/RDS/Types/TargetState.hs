{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.TargetState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.TargetState (
  TargetState (
    ..
    , TSAvailable
    , TSRegistering
    , TSUnavailable
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TargetState = TargetState' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern TSAvailable :: TargetState
pattern TSAvailable = TargetState' "AVAILABLE"

pattern TSRegistering :: TargetState
pattern TSRegistering = TargetState' "REGISTERING"

pattern TSUnavailable :: TargetState
pattern TSUnavailable = TargetState' "UNAVAILABLE"

{-# COMPLETE
  TSAvailable,
  TSRegistering,
  TSUnavailable,
  TargetState' #-}

instance FromText TargetState where
    parser = (TargetState' . mk) <$> takeText

instance ToText TargetState where
    toText (TargetState' ci) = original ci

-- | Represents an enum of /known/ $TargetState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TargetState where
    toEnum i = case i of
        0 -> TSAvailable
        1 -> TSRegistering
        2 -> TSUnavailable
        _ -> (error . showText) $ "Unknown index for TargetState: " <> toText i
    fromEnum x = case x of
        TSAvailable -> 0
        TSRegistering -> 1
        TSUnavailable -> 2
        TargetState' name -> (error . showText) $ "Unknown TargetState: " <> original name

-- | Represents the bounds of /known/ $TargetState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TargetState where
    minBound = TSAvailable
    maxBound = TSUnavailable

instance Hashable     TargetState
instance NFData       TargetState
instance ToByteString TargetState
instance ToQuery      TargetState
instance ToHeader     TargetState

instance FromXML TargetState where
    parseXML = parseXMLText "TargetState"
