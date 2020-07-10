{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.TargetHealthStateEnum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.TargetHealthStateEnum (
  TargetHealthStateEnum (
    ..
    , Draining
    , Healthy
    , Initial
    , Unavailable
    , Unhealthy
    , Unused
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TargetHealthStateEnum = TargetHealthStateEnum' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern Draining :: TargetHealthStateEnum
pattern Draining = TargetHealthStateEnum' "draining"

pattern Healthy :: TargetHealthStateEnum
pattern Healthy = TargetHealthStateEnum' "healthy"

pattern Initial :: TargetHealthStateEnum
pattern Initial = TargetHealthStateEnum' "initial"

pattern Unavailable :: TargetHealthStateEnum
pattern Unavailable = TargetHealthStateEnum' "unavailable"

pattern Unhealthy :: TargetHealthStateEnum
pattern Unhealthy = TargetHealthStateEnum' "unhealthy"

pattern Unused :: TargetHealthStateEnum
pattern Unused = TargetHealthStateEnum' "unused"

{-# COMPLETE
  Draining,
  Healthy,
  Initial,
  Unavailable,
  Unhealthy,
  Unused,
  TargetHealthStateEnum' #-}

instance FromText TargetHealthStateEnum where
    parser = (TargetHealthStateEnum' . mk) <$> takeText

instance ToText TargetHealthStateEnum where
    toText (TargetHealthStateEnum' ci) = original ci

-- | Represents an enum of /known/ $TargetHealthStateEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TargetHealthStateEnum where
    toEnum i = case i of
        0 -> Draining
        1 -> Healthy
        2 -> Initial
        3 -> Unavailable
        4 -> Unhealthy
        5 -> Unused
        _ -> (error . showText) $ "Unknown index for TargetHealthStateEnum: " <> toText i
    fromEnum x = case x of
        Draining -> 0
        Healthy -> 1
        Initial -> 2
        Unavailable -> 3
        Unhealthy -> 4
        Unused -> 5
        TargetHealthStateEnum' name -> (error . showText) $ "Unknown TargetHealthStateEnum: " <> original name

-- | Represents the bounds of /known/ $TargetHealthStateEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TargetHealthStateEnum where
    minBound = Draining
    maxBound = Unused

instance Hashable     TargetHealthStateEnum
instance NFData       TargetHealthStateEnum
instance ToByteString TargetHealthStateEnum
instance ToQuery      TargetHealthStateEnum
instance ToHeader     TargetHealthStateEnum

instance FromXML TargetHealthStateEnum where
    parseXML = parseXMLText "TargetHealthStateEnum"
