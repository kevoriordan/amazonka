{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.VirtualInterfaceState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.VirtualInterfaceState (
  VirtualInterfaceState (
    ..
    , VISAvailable
    , VISConfirming
    , VISDeleted
    , VISDeleting
    , VISDown
    , VISPending
    , VISRejected
    , VISUnknown
    , VISVerifying
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data VirtualInterfaceState = VirtualInterfaceState' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern VISAvailable :: VirtualInterfaceState
pattern VISAvailable = VirtualInterfaceState' "available"

pattern VISConfirming :: VirtualInterfaceState
pattern VISConfirming = VirtualInterfaceState' "confirming"

pattern VISDeleted :: VirtualInterfaceState
pattern VISDeleted = VirtualInterfaceState' "deleted"

pattern VISDeleting :: VirtualInterfaceState
pattern VISDeleting = VirtualInterfaceState' "deleting"

pattern VISDown :: VirtualInterfaceState
pattern VISDown = VirtualInterfaceState' "down"

pattern VISPending :: VirtualInterfaceState
pattern VISPending = VirtualInterfaceState' "pending"

pattern VISRejected :: VirtualInterfaceState
pattern VISRejected = VirtualInterfaceState' "rejected"

pattern VISUnknown :: VirtualInterfaceState
pattern VISUnknown = VirtualInterfaceState' "unknown"

pattern VISVerifying :: VirtualInterfaceState
pattern VISVerifying = VirtualInterfaceState' "verifying"

{-# COMPLETE
  VISAvailable,
  VISConfirming,
  VISDeleted,
  VISDeleting,
  VISDown,
  VISPending,
  VISRejected,
  VISUnknown,
  VISVerifying,
  VirtualInterfaceState' #-}

instance FromText VirtualInterfaceState where
    parser = (VirtualInterfaceState' . mk) <$> takeText

instance ToText VirtualInterfaceState where
    toText (VirtualInterfaceState' ci) = original ci

-- | Represents an enum of /known/ $VirtualInterfaceState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum VirtualInterfaceState where
    toEnum i = case i of
        0 -> VISAvailable
        1 -> VISConfirming
        2 -> VISDeleted
        3 -> VISDeleting
        4 -> VISDown
        5 -> VISPending
        6 -> VISRejected
        7 -> VISUnknown
        8 -> VISVerifying
        _ -> (error . showText) $ "Unknown index for VirtualInterfaceState: " <> toText i
    fromEnum x = case x of
        VISAvailable -> 0
        VISConfirming -> 1
        VISDeleted -> 2
        VISDeleting -> 3
        VISDown -> 4
        VISPending -> 5
        VISRejected -> 6
        VISUnknown -> 7
        VISVerifying -> 8
        VirtualInterfaceState' name -> (error . showText) $ "Unknown VirtualInterfaceState: " <> original name

-- | Represents the bounds of /known/ $VirtualInterfaceState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded VirtualInterfaceState where
    minBound = VISAvailable
    maxBound = VISVerifying

instance Hashable     VirtualInterfaceState
instance NFData       VirtualInterfaceState
instance ToByteString VirtualInterfaceState
instance ToQuery      VirtualInterfaceState
instance ToHeader     VirtualInterfaceState

instance FromJSON VirtualInterfaceState where
    parseJSON = parseJSONText "VirtualInterfaceState"
