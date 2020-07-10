{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPCPeeringConnectionStateReasonCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VPCPeeringConnectionStateReasonCode (
  VPCPeeringConnectionStateReasonCode (
    ..
    , VPCSRCActive
    , VPCSRCDeleted
    , VPCSRCDeleting
    , VPCSRCExpired
    , VPCSRCFailed
    , VPCSRCInitiatingRequest
    , VPCSRCPendingAcceptance
    , VPCSRCProvisioning
    , VPCSRCRejected
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VPCPeeringConnectionStateReasonCode = VPCPeeringConnectionStateReasonCode' (CI
                                                                                   Text)
                                             deriving (Eq, Ord, Read, Show,
                                                       Data, Typeable, Generic)

pattern VPCSRCActive :: VPCPeeringConnectionStateReasonCode
pattern VPCSRCActive = VPCPeeringConnectionStateReasonCode' "active"

pattern VPCSRCDeleted :: VPCPeeringConnectionStateReasonCode
pattern VPCSRCDeleted = VPCPeeringConnectionStateReasonCode' "deleted"

pattern VPCSRCDeleting :: VPCPeeringConnectionStateReasonCode
pattern VPCSRCDeleting = VPCPeeringConnectionStateReasonCode' "deleting"

pattern VPCSRCExpired :: VPCPeeringConnectionStateReasonCode
pattern VPCSRCExpired = VPCPeeringConnectionStateReasonCode' "expired"

pattern VPCSRCFailed :: VPCPeeringConnectionStateReasonCode
pattern VPCSRCFailed = VPCPeeringConnectionStateReasonCode' "failed"

pattern VPCSRCInitiatingRequest :: VPCPeeringConnectionStateReasonCode
pattern VPCSRCInitiatingRequest = VPCPeeringConnectionStateReasonCode' "initiating-request"

pattern VPCSRCPendingAcceptance :: VPCPeeringConnectionStateReasonCode
pattern VPCSRCPendingAcceptance = VPCPeeringConnectionStateReasonCode' "pending-acceptance"

pattern VPCSRCProvisioning :: VPCPeeringConnectionStateReasonCode
pattern VPCSRCProvisioning = VPCPeeringConnectionStateReasonCode' "provisioning"

pattern VPCSRCRejected :: VPCPeeringConnectionStateReasonCode
pattern VPCSRCRejected = VPCPeeringConnectionStateReasonCode' "rejected"

{-# COMPLETE
  VPCSRCActive,
  VPCSRCDeleted,
  VPCSRCDeleting,
  VPCSRCExpired,
  VPCSRCFailed,
  VPCSRCInitiatingRequest,
  VPCSRCPendingAcceptance,
  VPCSRCProvisioning,
  VPCSRCRejected,
  VPCPeeringConnectionStateReasonCode' #-}

instance FromText VPCPeeringConnectionStateReasonCode where
    parser = (VPCPeeringConnectionStateReasonCode' . mk) <$> takeText

instance ToText VPCPeeringConnectionStateReasonCode where
    toText (VPCPeeringConnectionStateReasonCode' ci) = original ci

-- | Represents an enum of /known/ $VPCPeeringConnectionStateReasonCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum VPCPeeringConnectionStateReasonCode where
    toEnum i = case i of
        0 -> VPCSRCActive
        1 -> VPCSRCDeleted
        2 -> VPCSRCDeleting
        3 -> VPCSRCExpired
        4 -> VPCSRCFailed
        5 -> VPCSRCInitiatingRequest
        6 -> VPCSRCPendingAcceptance
        7 -> VPCSRCProvisioning
        8 -> VPCSRCRejected
        _ -> (error . showText) $ "Unknown index for VPCPeeringConnectionStateReasonCode: " <> toText i
    fromEnum x = case x of
        VPCSRCActive -> 0
        VPCSRCDeleted -> 1
        VPCSRCDeleting -> 2
        VPCSRCExpired -> 3
        VPCSRCFailed -> 4
        VPCSRCInitiatingRequest -> 5
        VPCSRCPendingAcceptance -> 6
        VPCSRCProvisioning -> 7
        VPCSRCRejected -> 8
        VPCPeeringConnectionStateReasonCode' name -> (error . showText) $ "Unknown VPCPeeringConnectionStateReasonCode: " <> original name

-- | Represents the bounds of /known/ $VPCPeeringConnectionStateReasonCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded VPCPeeringConnectionStateReasonCode where
    minBound = VPCSRCActive
    maxBound = VPCSRCRejected

instance Hashable     VPCPeeringConnectionStateReasonCode
instance NFData       VPCPeeringConnectionStateReasonCode
instance ToByteString VPCPeeringConnectionStateReasonCode
instance ToQuery      VPCPeeringConnectionStateReasonCode
instance ToHeader     VPCPeeringConnectionStateReasonCode

instance FromXML VPCPeeringConnectionStateReasonCode where
    parseXML = parseXMLText "VPCPeeringConnectionStateReasonCode"
