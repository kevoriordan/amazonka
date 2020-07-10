{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPNState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VPNState (
  VPNState (
    ..
    , VSAvailable
    , VSDeleted
    , VSDeleting
    , VSPending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VPNState = VPNState' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern VSAvailable :: VPNState
pattern VSAvailable = VPNState' "available"

pattern VSDeleted :: VPNState
pattern VSDeleted = VPNState' "deleted"

pattern VSDeleting :: VPNState
pattern VSDeleting = VPNState' "deleting"

pattern VSPending :: VPNState
pattern VSPending = VPNState' "pending"

{-# COMPLETE
  VSAvailable,
  VSDeleted,
  VSDeleting,
  VSPending,
  VPNState' #-}

instance FromText VPNState where
    parser = (VPNState' . mk) <$> takeText

instance ToText VPNState where
    toText (VPNState' ci) = original ci

-- | Represents an enum of /known/ $VPNState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum VPNState where
    toEnum i = case i of
        0 -> VSAvailable
        1 -> VSDeleted
        2 -> VSDeleting
        3 -> VSPending
        _ -> (error . showText) $ "Unknown index for VPNState: " <> toText i
    fromEnum x = case x of
        VSAvailable -> 0
        VSDeleted -> 1
        VSDeleting -> 2
        VSPending -> 3
        VPNState' name -> (error . showText) $ "Unknown VPNState: " <> original name

-- | Represents the bounds of /known/ $VPNState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded VPNState where
    minBound = VSAvailable
    maxBound = VSPending

instance Hashable     VPNState
instance NFData       VPNState
instance ToByteString VPNState
instance ToQuery      VPNState
instance ToHeader     VPNState

instance FromXML VPNState where
    parseXML = parseXMLText "VPNState"
