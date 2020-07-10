{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayAttachmentState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TransitGatewayAttachmentState (
  TransitGatewayAttachmentState (
    ..
    , TGASAvailable
    , TGASDeleted
    , TGASDeleting
    , TGASFailed
    , TGASFailing
    , TGASInitiating
    , TGASModifying
    , TGASPending
    , TGASPendingAcceptance
    , TGASRejected
    , TGASRejecting
    , TGASRollingBack
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TransitGatewayAttachmentState = TransitGatewayAttachmentState' (CI
                                                                       Text)
                                       deriving (Eq, Ord, Read, Show, Data,
                                                 Typeable, Generic)

pattern TGASAvailable :: TransitGatewayAttachmentState
pattern TGASAvailable = TransitGatewayAttachmentState' "available"

pattern TGASDeleted :: TransitGatewayAttachmentState
pattern TGASDeleted = TransitGatewayAttachmentState' "deleted"

pattern TGASDeleting :: TransitGatewayAttachmentState
pattern TGASDeleting = TransitGatewayAttachmentState' "deleting"

pattern TGASFailed :: TransitGatewayAttachmentState
pattern TGASFailed = TransitGatewayAttachmentState' "failed"

pattern TGASFailing :: TransitGatewayAttachmentState
pattern TGASFailing = TransitGatewayAttachmentState' "failing"

pattern TGASInitiating :: TransitGatewayAttachmentState
pattern TGASInitiating = TransitGatewayAttachmentState' "initiating"

pattern TGASModifying :: TransitGatewayAttachmentState
pattern TGASModifying = TransitGatewayAttachmentState' "modifying"

pattern TGASPending :: TransitGatewayAttachmentState
pattern TGASPending = TransitGatewayAttachmentState' "pending"

pattern TGASPendingAcceptance :: TransitGatewayAttachmentState
pattern TGASPendingAcceptance = TransitGatewayAttachmentState' "pendingAcceptance"

pattern TGASRejected :: TransitGatewayAttachmentState
pattern TGASRejected = TransitGatewayAttachmentState' "rejected"

pattern TGASRejecting :: TransitGatewayAttachmentState
pattern TGASRejecting = TransitGatewayAttachmentState' "rejecting"

pattern TGASRollingBack :: TransitGatewayAttachmentState
pattern TGASRollingBack = TransitGatewayAttachmentState' "rollingBack"

{-# COMPLETE
  TGASAvailable,
  TGASDeleted,
  TGASDeleting,
  TGASFailed,
  TGASFailing,
  TGASInitiating,
  TGASModifying,
  TGASPending,
  TGASPendingAcceptance,
  TGASRejected,
  TGASRejecting,
  TGASRollingBack,
  TransitGatewayAttachmentState' #-}

instance FromText TransitGatewayAttachmentState where
    parser = (TransitGatewayAttachmentState' . mk) <$> takeText

instance ToText TransitGatewayAttachmentState where
    toText (TransitGatewayAttachmentState' ci) = original ci

-- | Represents an enum of /known/ $TransitGatewayAttachmentState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TransitGatewayAttachmentState where
    toEnum i = case i of
        0 -> TGASAvailable
        1 -> TGASDeleted
        2 -> TGASDeleting
        3 -> TGASFailed
        4 -> TGASFailing
        5 -> TGASInitiating
        6 -> TGASModifying
        7 -> TGASPending
        8 -> TGASPendingAcceptance
        9 -> TGASRejected
        10 -> TGASRejecting
        11 -> TGASRollingBack
        _ -> (error . showText) $ "Unknown index for TransitGatewayAttachmentState: " <> toText i
    fromEnum x = case x of
        TGASAvailable -> 0
        TGASDeleted -> 1
        TGASDeleting -> 2
        TGASFailed -> 3
        TGASFailing -> 4
        TGASInitiating -> 5
        TGASModifying -> 6
        TGASPending -> 7
        TGASPendingAcceptance -> 8
        TGASRejected -> 9
        TGASRejecting -> 10
        TGASRollingBack -> 11
        TransitGatewayAttachmentState' name -> (error . showText) $ "Unknown TransitGatewayAttachmentState: " <> original name

-- | Represents the bounds of /known/ $TransitGatewayAttachmentState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TransitGatewayAttachmentState where
    minBound = TGASAvailable
    maxBound = TGASRollingBack

instance Hashable     TransitGatewayAttachmentState
instance NFData       TransitGatewayAttachmentState
instance ToByteString TransitGatewayAttachmentState
instance ToQuery      TransitGatewayAttachmentState
instance ToHeader     TransitGatewayAttachmentState

instance FromXML TransitGatewayAttachmentState where
    parseXML = parseXMLText "TransitGatewayAttachmentState"
