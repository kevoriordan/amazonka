{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TransitGatewayState (
  TransitGatewayState (
    ..
    , TGSAvailable
    , TGSDeleted
    , TGSDeleting
    , TGSModifying
    , TGSPending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TransitGatewayState = TransitGatewayState' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern TGSAvailable :: TransitGatewayState
pattern TGSAvailable = TransitGatewayState' "available"

pattern TGSDeleted :: TransitGatewayState
pattern TGSDeleted = TransitGatewayState' "deleted"

pattern TGSDeleting :: TransitGatewayState
pattern TGSDeleting = TransitGatewayState' "deleting"

pattern TGSModifying :: TransitGatewayState
pattern TGSModifying = TransitGatewayState' "modifying"

pattern TGSPending :: TransitGatewayState
pattern TGSPending = TransitGatewayState' "pending"

{-# COMPLETE
  TGSAvailable,
  TGSDeleted,
  TGSDeleting,
  TGSModifying,
  TGSPending,
  TransitGatewayState' #-}

instance FromText TransitGatewayState where
    parser = (TransitGatewayState' . mk) <$> takeText

instance ToText TransitGatewayState where
    toText (TransitGatewayState' ci) = original ci

-- | Represents an enum of /known/ $TransitGatewayState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TransitGatewayState where
    toEnum i = case i of
        0 -> TGSAvailable
        1 -> TGSDeleted
        2 -> TGSDeleting
        3 -> TGSModifying
        4 -> TGSPending
        _ -> (error . showText) $ "Unknown index for TransitGatewayState: " <> toText i
    fromEnum x = case x of
        TGSAvailable -> 0
        TGSDeleted -> 1
        TGSDeleting -> 2
        TGSModifying -> 3
        TGSPending -> 4
        TransitGatewayState' name -> (error . showText) $ "Unknown TransitGatewayState: " <> original name

-- | Represents the bounds of /known/ $TransitGatewayState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TransitGatewayState where
    minBound = TGSAvailable
    maxBound = TGSPending

instance Hashable     TransitGatewayState
instance NFData       TransitGatewayState
instance ToByteString TransitGatewayState
instance ToQuery      TransitGatewayState
instance ToHeader     TransitGatewayState

instance FromXML TransitGatewayState where
    parseXML = parseXMLText "TransitGatewayState"
