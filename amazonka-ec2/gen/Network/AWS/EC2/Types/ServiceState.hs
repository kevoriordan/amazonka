{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ServiceState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ServiceState (
  ServiceState (
    ..
    , SerAvailable
    , SerDeleted
    , SerDeleting
    , SerFailed
    , SerPending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ServiceState = ServiceState' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern SerAvailable :: ServiceState
pattern SerAvailable = ServiceState' "Available"

pattern SerDeleted :: ServiceState
pattern SerDeleted = ServiceState' "Deleted"

pattern SerDeleting :: ServiceState
pattern SerDeleting = ServiceState' "Deleting"

pattern SerFailed :: ServiceState
pattern SerFailed = ServiceState' "Failed"

pattern SerPending :: ServiceState
pattern SerPending = ServiceState' "Pending"

{-# COMPLETE
  SerAvailable,
  SerDeleted,
  SerDeleting,
  SerFailed,
  SerPending,
  ServiceState' #-}

instance FromText ServiceState where
    parser = (ServiceState' . mk) <$> takeText

instance ToText ServiceState where
    toText (ServiceState' ci) = original ci

-- | Represents an enum of /known/ $ServiceState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ServiceState where
    toEnum i = case i of
        0 -> SerAvailable
        1 -> SerDeleted
        2 -> SerDeleting
        3 -> SerFailed
        4 -> SerPending
        _ -> (error . showText) $ "Unknown index for ServiceState: " <> toText i
    fromEnum x = case x of
        SerAvailable -> 0
        SerDeleted -> 1
        SerDeleting -> 2
        SerFailed -> 3
        SerPending -> 4
        ServiceState' name -> (error . showText) $ "Unknown ServiceState: " <> original name

-- | Represents the bounds of /known/ $ServiceState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ServiceState where
    minBound = SerAvailable
    maxBound = SerPending

instance Hashable     ServiceState
instance NFData       ServiceState
instance ToByteString ServiceState
instance ToQuery      ServiceState
instance ToHeader     ServiceState

instance FromXML ServiceState where
    parseXML = parseXMLText "ServiceState"
