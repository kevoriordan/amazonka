{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulitcastDomainAssociationState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TransitGatewayMulitcastDomainAssociationState (
  TransitGatewayMulitcastDomainAssociationState (
    ..
    , Associated
    , Associating
    , Disassociated
    , Disassociating
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TransitGatewayMulitcastDomainAssociationState = TransitGatewayMulitcastDomainAssociationState' (CI
                                                                                                       Text)
                                                       deriving (Eq, Ord, Read,
                                                                 Show, Data,
                                                                 Typeable,
                                                                 Generic)

pattern Associated :: TransitGatewayMulitcastDomainAssociationState
pattern Associated = TransitGatewayMulitcastDomainAssociationState' "associated"

pattern Associating :: TransitGatewayMulitcastDomainAssociationState
pattern Associating = TransitGatewayMulitcastDomainAssociationState' "associating"

pattern Disassociated :: TransitGatewayMulitcastDomainAssociationState
pattern Disassociated = TransitGatewayMulitcastDomainAssociationState' "disassociated"

pattern Disassociating :: TransitGatewayMulitcastDomainAssociationState
pattern Disassociating = TransitGatewayMulitcastDomainAssociationState' "disassociating"

{-# COMPLETE
  Associated,
  Associating,
  Disassociated,
  Disassociating,
  TransitGatewayMulitcastDomainAssociationState' #-}

instance FromText TransitGatewayMulitcastDomainAssociationState where
    parser = (TransitGatewayMulitcastDomainAssociationState' . mk) <$> takeText

instance ToText TransitGatewayMulitcastDomainAssociationState where
    toText (TransitGatewayMulitcastDomainAssociationState' ci) = original ci

-- | Represents an enum of /known/ $TransitGatewayMulitcastDomainAssociationState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TransitGatewayMulitcastDomainAssociationState where
    toEnum i = case i of
        0 -> Associated
        1 -> Associating
        2 -> Disassociated
        3 -> Disassociating
        _ -> (error . showText) $ "Unknown index for TransitGatewayMulitcastDomainAssociationState: " <> toText i
    fromEnum x = case x of
        Associated -> 0
        Associating -> 1
        Disassociated -> 2
        Disassociating -> 3
        TransitGatewayMulitcastDomainAssociationState' name -> (error . showText) $ "Unknown TransitGatewayMulitcastDomainAssociationState: " <> original name

-- | Represents the bounds of /known/ $TransitGatewayMulitcastDomainAssociationState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TransitGatewayMulitcastDomainAssociationState where
    minBound = Associated
    maxBound = Disassociating

instance Hashable     TransitGatewayMulitcastDomainAssociationState
instance NFData       TransitGatewayMulitcastDomainAssociationState
instance ToByteString TransitGatewayMulitcastDomainAssociationState
instance ToQuery      TransitGatewayMulitcastDomainAssociationState
instance ToHeader     TransitGatewayMulitcastDomainAssociationState

instance FromXML TransitGatewayMulitcastDomainAssociationState where
    parseXML = parseXMLText "TransitGatewayMulitcastDomainAssociationState"
