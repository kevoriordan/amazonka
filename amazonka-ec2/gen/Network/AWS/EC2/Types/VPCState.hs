{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPCState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VPCState (
  VPCState (
    ..
    , VPCSAvailable
    , VPCSPending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VPCState = VPCState' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern VPCSAvailable :: VPCState
pattern VPCSAvailable = VPCState' "available"

pattern VPCSPending :: VPCState
pattern VPCSPending = VPCState' "pending"

{-# COMPLETE
  VPCSAvailable,
  VPCSPending,
  VPCState' #-}

instance FromText VPCState where
    parser = (VPCState' . mk) <$> takeText

instance ToText VPCState where
    toText (VPCState' ci) = original ci

-- | Represents an enum of /known/ $VPCState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum VPCState where
    toEnum i = case i of
        0 -> VPCSAvailable
        1 -> VPCSPending
        _ -> (error . showText) $ "Unknown index for VPCState: " <> toText i
    fromEnum x = case x of
        VPCSAvailable -> 0
        VPCSPending -> 1
        VPCState' name -> (error . showText) $ "Unknown VPCState: " <> original name

-- | Represents the bounds of /known/ $VPCState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded VPCState where
    minBound = VPCSAvailable
    maxBound = VPCSPending

instance Hashable     VPCState
instance NFData       VPCState
instance ToByteString VPCState
instance ToQuery      VPCState
instance ToHeader     VPCState

instance FromXML VPCState where
    parseXML = parseXMLText "VPCState"
