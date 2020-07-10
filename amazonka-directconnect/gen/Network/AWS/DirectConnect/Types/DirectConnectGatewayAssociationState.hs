{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationState (
  DirectConnectGatewayAssociationState (
    ..
    , Associated
    , Associating
    , Disassociated
    , Disassociating
    , Updating
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DirectConnectGatewayAssociationState = DirectConnectGatewayAssociationState' (CI
                                                                                     Text)
                                              deriving (Eq, Ord, Read, Show,
                                                        Data, Typeable, Generic)

pattern Associated :: DirectConnectGatewayAssociationState
pattern Associated = DirectConnectGatewayAssociationState' "associated"

pattern Associating :: DirectConnectGatewayAssociationState
pattern Associating = DirectConnectGatewayAssociationState' "associating"

pattern Disassociated :: DirectConnectGatewayAssociationState
pattern Disassociated = DirectConnectGatewayAssociationState' "disassociated"

pattern Disassociating :: DirectConnectGatewayAssociationState
pattern Disassociating = DirectConnectGatewayAssociationState' "disassociating"

pattern Updating :: DirectConnectGatewayAssociationState
pattern Updating = DirectConnectGatewayAssociationState' "updating"

{-# COMPLETE
  Associated,
  Associating,
  Disassociated,
  Disassociating,
  Updating,
  DirectConnectGatewayAssociationState' #-}

instance FromText DirectConnectGatewayAssociationState where
    parser = (DirectConnectGatewayAssociationState' . mk) <$> takeText

instance ToText DirectConnectGatewayAssociationState where
    toText (DirectConnectGatewayAssociationState' ci) = original ci

-- | Represents an enum of /known/ $DirectConnectGatewayAssociationState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DirectConnectGatewayAssociationState where
    toEnum i = case i of
        0 -> Associated
        1 -> Associating
        2 -> Disassociated
        3 -> Disassociating
        4 -> Updating
        _ -> (error . showText) $ "Unknown index for DirectConnectGatewayAssociationState: " <> toText i
    fromEnum x = case x of
        Associated -> 0
        Associating -> 1
        Disassociated -> 2
        Disassociating -> 3
        Updating -> 4
        DirectConnectGatewayAssociationState' name -> (error . showText) $ "Unknown DirectConnectGatewayAssociationState: " <> original name

-- | Represents the bounds of /known/ $DirectConnectGatewayAssociationState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DirectConnectGatewayAssociationState where
    minBound = Associated
    maxBound = Updating

instance Hashable     DirectConnectGatewayAssociationState
instance NFData       DirectConnectGatewayAssociationState
instance ToByteString DirectConnectGatewayAssociationState
instance ToQuery      DirectConnectGatewayAssociationState
instance ToHeader     DirectConnectGatewayAssociationState

instance FromJSON DirectConnectGatewayAssociationState where
    parseJSON = parseJSONText "DirectConnectGatewayAssociationState"
