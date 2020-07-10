{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayRouteState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TransitGatewayRouteState (
  TransitGatewayRouteState (
    ..
    , TGRSActive
    , TGRSBlackhole
    , TGRSDeleted
    , TGRSDeleting
    , TGRSPending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TransitGatewayRouteState = TransitGatewayRouteState' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern TGRSActive :: TransitGatewayRouteState
pattern TGRSActive = TransitGatewayRouteState' "active"

pattern TGRSBlackhole :: TransitGatewayRouteState
pattern TGRSBlackhole = TransitGatewayRouteState' "blackhole"

pattern TGRSDeleted :: TransitGatewayRouteState
pattern TGRSDeleted = TransitGatewayRouteState' "deleted"

pattern TGRSDeleting :: TransitGatewayRouteState
pattern TGRSDeleting = TransitGatewayRouteState' "deleting"

pattern TGRSPending :: TransitGatewayRouteState
pattern TGRSPending = TransitGatewayRouteState' "pending"

{-# COMPLETE
  TGRSActive,
  TGRSBlackhole,
  TGRSDeleted,
  TGRSDeleting,
  TGRSPending,
  TransitGatewayRouteState' #-}

instance FromText TransitGatewayRouteState where
    parser = (TransitGatewayRouteState' . mk) <$> takeText

instance ToText TransitGatewayRouteState where
    toText (TransitGatewayRouteState' ci) = original ci

-- | Represents an enum of /known/ $TransitGatewayRouteState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TransitGatewayRouteState where
    toEnum i = case i of
        0 -> TGRSActive
        1 -> TGRSBlackhole
        2 -> TGRSDeleted
        3 -> TGRSDeleting
        4 -> TGRSPending
        _ -> (error . showText) $ "Unknown index for TransitGatewayRouteState: " <> toText i
    fromEnum x = case x of
        TGRSActive -> 0
        TGRSBlackhole -> 1
        TGRSDeleted -> 2
        TGRSDeleting -> 3
        TGRSPending -> 4
        TransitGatewayRouteState' name -> (error . showText) $ "Unknown TransitGatewayRouteState: " <> original name

-- | Represents the bounds of /known/ $TransitGatewayRouteState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TransitGatewayRouteState where
    minBound = TGRSActive
    maxBound = TGRSPending

instance Hashable     TransitGatewayRouteState
instance NFData       TransitGatewayRouteState
instance ToByteString TransitGatewayRouteState
instance ToQuery      TransitGatewayRouteState
instance ToHeader     TransitGatewayRouteState

instance FromXML TransitGatewayRouteState where
    parseXML = parseXMLText "TransitGatewayRouteState"
