{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBProxyStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.DBProxyStatus (
  DBProxyStatus (
    ..
    , Available
    , Creating
    , Deleting
    , IncompatibleNetwork
    , InsufficientResourceLimits
    , Modifying
    , Reactivating
    , Suspended
    , Suspending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DBProxyStatus = DBProxyStatus' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern Available :: DBProxyStatus
pattern Available = DBProxyStatus' "available"

pattern Creating :: DBProxyStatus
pattern Creating = DBProxyStatus' "creating"

pattern Deleting :: DBProxyStatus
pattern Deleting = DBProxyStatus' "deleting"

pattern IncompatibleNetwork :: DBProxyStatus
pattern IncompatibleNetwork = DBProxyStatus' "incompatible-network"

pattern InsufficientResourceLimits :: DBProxyStatus
pattern InsufficientResourceLimits = DBProxyStatus' "insufficient-resource-limits"

pattern Modifying :: DBProxyStatus
pattern Modifying = DBProxyStatus' "modifying"

pattern Reactivating :: DBProxyStatus
pattern Reactivating = DBProxyStatus' "reactivating"

pattern Suspended :: DBProxyStatus
pattern Suspended = DBProxyStatus' "suspended"

pattern Suspending :: DBProxyStatus
pattern Suspending = DBProxyStatus' "suspending"

{-# COMPLETE
  Available,
  Creating,
  Deleting,
  IncompatibleNetwork,
  InsufficientResourceLimits,
  Modifying,
  Reactivating,
  Suspended,
  Suspending,
  DBProxyStatus' #-}

instance FromText DBProxyStatus where
    parser = (DBProxyStatus' . mk) <$> takeText

instance ToText DBProxyStatus where
    toText (DBProxyStatus' ci) = original ci

-- | Represents an enum of /known/ $DBProxyStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DBProxyStatus where
    toEnum i = case i of
        0 -> Available
        1 -> Creating
        2 -> Deleting
        3 -> IncompatibleNetwork
        4 -> InsufficientResourceLimits
        5 -> Modifying
        6 -> Reactivating
        7 -> Suspended
        8 -> Suspending
        _ -> (error . showText) $ "Unknown index for DBProxyStatus: " <> toText i
    fromEnum x = case x of
        Available -> 0
        Creating -> 1
        Deleting -> 2
        IncompatibleNetwork -> 3
        InsufficientResourceLimits -> 4
        Modifying -> 5
        Reactivating -> 6
        Suspended -> 7
        Suspending -> 8
        DBProxyStatus' name -> (error . showText) $ "Unknown DBProxyStatus: " <> original name

-- | Represents the bounds of /known/ $DBProxyStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DBProxyStatus where
    minBound = Available
    maxBound = Suspending

instance Hashable     DBProxyStatus
instance NFData       DBProxyStatus
instance ToByteString DBProxyStatus
instance ToQuery      DBProxyStatus
instance ToHeader     DBProxyStatus

instance FromXML DBProxyStatus where
    parseXML = parseXMLText "DBProxyStatus"
