{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationProposalState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationProposalState (
  DirectConnectGatewayAssociationProposalState (
    ..
    , DCGAPSAccepted
    , DCGAPSDeleted
    , DCGAPSRequested
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DirectConnectGatewayAssociationProposalState = DirectConnectGatewayAssociationProposalState' (CI
                                                                                                     Text)
                                                      deriving (Eq, Ord, Read,
                                                                Show, Data,
                                                                Typeable,
                                                                Generic)

pattern DCGAPSAccepted :: DirectConnectGatewayAssociationProposalState
pattern DCGAPSAccepted = DirectConnectGatewayAssociationProposalState' "accepted"

pattern DCGAPSDeleted :: DirectConnectGatewayAssociationProposalState
pattern DCGAPSDeleted = DirectConnectGatewayAssociationProposalState' "deleted"

pattern DCGAPSRequested :: DirectConnectGatewayAssociationProposalState
pattern DCGAPSRequested = DirectConnectGatewayAssociationProposalState' "requested"

{-# COMPLETE
  DCGAPSAccepted,
  DCGAPSDeleted,
  DCGAPSRequested,
  DirectConnectGatewayAssociationProposalState' #-}

instance FromText DirectConnectGatewayAssociationProposalState where
    parser = (DirectConnectGatewayAssociationProposalState' . mk) <$> takeText

instance ToText DirectConnectGatewayAssociationProposalState where
    toText (DirectConnectGatewayAssociationProposalState' ci) = original ci

-- | Represents an enum of /known/ $DirectConnectGatewayAssociationProposalState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DirectConnectGatewayAssociationProposalState where
    toEnum i = case i of
        0 -> DCGAPSAccepted
        1 -> DCGAPSDeleted
        2 -> DCGAPSRequested
        _ -> (error . showText) $ "Unknown index for DirectConnectGatewayAssociationProposalState: " <> toText i
    fromEnum x = case x of
        DCGAPSAccepted -> 0
        DCGAPSDeleted -> 1
        DCGAPSRequested -> 2
        DirectConnectGatewayAssociationProposalState' name -> (error . showText) $ "Unknown DirectConnectGatewayAssociationProposalState: " <> original name

-- | Represents the bounds of /known/ $DirectConnectGatewayAssociationProposalState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DirectConnectGatewayAssociationProposalState where
    minBound = DCGAPSAccepted
    maxBound = DCGAPSRequested

instance Hashable     DirectConnectGatewayAssociationProposalState
instance NFData       DirectConnectGatewayAssociationProposalState
instance ToByteString DirectConnectGatewayAssociationProposalState
instance ToQuery      DirectConnectGatewayAssociationProposalState
instance ToHeader     DirectConnectGatewayAssociationProposalState

instance FromJSON DirectConnectGatewayAssociationProposalState where
    parseJSON = parseJSONText "DirectConnectGatewayAssociationProposalState"
