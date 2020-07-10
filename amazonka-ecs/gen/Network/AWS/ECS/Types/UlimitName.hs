{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.UlimitName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.UlimitName (
  UlimitName (
    ..
    , CPU
    , Core
    , Data
    , Fsize
    , Locks
    , Memlock
    , Msgqueue
    , Nice
    , Nofile
    , Nproc
    , Rss
    , Rtprio
    , Rttime
    , Sigpending
    , Stack
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data UlimitName = UlimitName' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern CPU :: UlimitName
pattern CPU = UlimitName' "cpu"

pattern Core :: UlimitName
pattern Core = UlimitName' "core"

pattern Data :: UlimitName
pattern Data = UlimitName' "data"

pattern Fsize :: UlimitName
pattern Fsize = UlimitName' "fsize"

pattern Locks :: UlimitName
pattern Locks = UlimitName' "locks"

pattern Memlock :: UlimitName
pattern Memlock = UlimitName' "memlock"

pattern Msgqueue :: UlimitName
pattern Msgqueue = UlimitName' "msgqueue"

pattern Nice :: UlimitName
pattern Nice = UlimitName' "nice"

pattern Nofile :: UlimitName
pattern Nofile = UlimitName' "nofile"

pattern Nproc :: UlimitName
pattern Nproc = UlimitName' "nproc"

pattern Rss :: UlimitName
pattern Rss = UlimitName' "rss"

pattern Rtprio :: UlimitName
pattern Rtprio = UlimitName' "rtprio"

pattern Rttime :: UlimitName
pattern Rttime = UlimitName' "rttime"

pattern Sigpending :: UlimitName
pattern Sigpending = UlimitName' "sigpending"

pattern Stack :: UlimitName
pattern Stack = UlimitName' "stack"

{-# COMPLETE
  CPU,
  Core,
  Data,
  Fsize,
  Locks,
  Memlock,
  Msgqueue,
  Nice,
  Nofile,
  Nproc,
  Rss,
  Rtprio,
  Rttime,
  Sigpending,
  Stack,
  UlimitName' #-}

instance FromText UlimitName where
    parser = (UlimitName' . mk) <$> takeText

instance ToText UlimitName where
    toText (UlimitName' ci) = original ci

-- | Represents an enum of /known/ $UlimitName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum UlimitName where
    toEnum i = case i of
        0 -> CPU
        1 -> Core
        2 -> Data
        3 -> Fsize
        4 -> Locks
        5 -> Memlock
        6 -> Msgqueue
        7 -> Nice
        8 -> Nofile
        9 -> Nproc
        10 -> Rss
        11 -> Rtprio
        12 -> Rttime
        13 -> Sigpending
        14 -> Stack
        _ -> (error . showText) $ "Unknown index for UlimitName: " <> toText i
    fromEnum x = case x of
        CPU -> 0
        Core -> 1
        Data -> 2
        Fsize -> 3
        Locks -> 4
        Memlock -> 5
        Msgqueue -> 6
        Nice -> 7
        Nofile -> 8
        Nproc -> 9
        Rss -> 10
        Rtprio -> 11
        Rttime -> 12
        Sigpending -> 13
        Stack -> 14
        UlimitName' name -> (error . showText) $ "Unknown UlimitName: " <> original name

-- | Represents the bounds of /known/ $UlimitName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded UlimitName where
    minBound = CPU
    maxBound = Stack

instance Hashable     UlimitName
instance NFData       UlimitName
instance ToByteString UlimitName
instance ToQuery      UlimitName
instance ToHeader     UlimitName

instance ToJSON UlimitName where
    toJSON = toJSONText

instance FromJSON UlimitName where
    parseJSON = parseJSONText "UlimitName"
