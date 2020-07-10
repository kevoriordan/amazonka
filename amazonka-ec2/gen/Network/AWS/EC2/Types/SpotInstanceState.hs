{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotInstanceState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.SpotInstanceState (
  SpotInstanceState (
    ..
    , SISActive
    , SISCancelled
    , SISClosed
    , SISFailed
    , SISOpen
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data SpotInstanceState = SpotInstanceState' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern SISActive :: SpotInstanceState
pattern SISActive = SpotInstanceState' "active"

pattern SISCancelled :: SpotInstanceState
pattern SISCancelled = SpotInstanceState' "cancelled"

pattern SISClosed :: SpotInstanceState
pattern SISClosed = SpotInstanceState' "closed"

pattern SISFailed :: SpotInstanceState
pattern SISFailed = SpotInstanceState' "failed"

pattern SISOpen :: SpotInstanceState
pattern SISOpen = SpotInstanceState' "open"

{-# COMPLETE
  SISActive,
  SISCancelled,
  SISClosed,
  SISFailed,
  SISOpen,
  SpotInstanceState' #-}

instance FromText SpotInstanceState where
    parser = (SpotInstanceState' . mk) <$> takeText

instance ToText SpotInstanceState where
    toText (SpotInstanceState' ci) = original ci

-- | Represents an enum of /known/ $SpotInstanceState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SpotInstanceState where
    toEnum i = case i of
        0 -> SISActive
        1 -> SISCancelled
        2 -> SISClosed
        3 -> SISFailed
        4 -> SISOpen
        _ -> (error . showText) $ "Unknown index for SpotInstanceState: " <> toText i
    fromEnum x = case x of
        SISActive -> 0
        SISCancelled -> 1
        SISClosed -> 2
        SISFailed -> 3
        SISOpen -> 4
        SpotInstanceState' name -> (error . showText) $ "Unknown SpotInstanceState: " <> original name

-- | Represents the bounds of /known/ $SpotInstanceState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SpotInstanceState where
    minBound = SISActive
    maxBound = SISOpen

instance Hashable     SpotInstanceState
instance NFData       SpotInstanceState
instance ToByteString SpotInstanceState
instance ToQuery      SpotInstanceState
instance ToHeader     SpotInstanceState

instance FromXML SpotInstanceState where
    parseXML = parseXMLText "SpotInstanceState"
