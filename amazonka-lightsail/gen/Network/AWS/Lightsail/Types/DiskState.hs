{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.DiskState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.DiskState (
  DiskState (
    ..
    , Available
    , Error'
    , InUse
    , Pending
    , Unknown
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DiskState = DiskState' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern Available :: DiskState
pattern Available = DiskState' "available"

pattern Error' :: DiskState
pattern Error' = DiskState' "error"

pattern InUse :: DiskState
pattern InUse = DiskState' "in-use"

pattern Pending :: DiskState
pattern Pending = DiskState' "pending"

pattern Unknown :: DiskState
pattern Unknown = DiskState' "unknown"

{-# COMPLETE
  Available,
  Error',
  InUse,
  Pending,
  Unknown,
  DiskState' #-}

instance FromText DiskState where
    parser = (DiskState' . mk) <$> takeText

instance ToText DiskState where
    toText (DiskState' ci) = original ci

-- | Represents an enum of /known/ $DiskState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DiskState where
    toEnum i = case i of
        0 -> Available
        1 -> Error'
        2 -> InUse
        3 -> Pending
        4 -> Unknown
        _ -> (error . showText) $ "Unknown index for DiskState: " <> toText i
    fromEnum x = case x of
        Available -> 0
        Error' -> 1
        InUse -> 2
        Pending -> 3
        Unknown -> 4
        DiskState' name -> (error . showText) $ "Unknown DiskState: " <> original name

-- | Represents the bounds of /known/ $DiskState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DiskState where
    minBound = Available
    maxBound = Unknown

instance Hashable     DiskState
instance NFData       DiskState
instance ToByteString DiskState
instance ToQuery      DiskState
instance ToHeader     DiskState

instance FromJSON DiskState where
    parseJSON = parseJSONText "DiskState"
