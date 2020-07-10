{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVPNEndpointStatusCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ClientVPNEndpointStatusCode (
  ClientVPNEndpointStatusCode (
    ..
    , CVESCAvailable
    , CVESCDeleted
    , CVESCDeleting
    , CVESCPendingAssociate
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ClientVPNEndpointStatusCode = ClientVPNEndpointStatusCode' (CI
                                                                   Text)
                                     deriving (Eq, Ord, Read, Show, Data,
                                               Typeable, Generic)

pattern CVESCAvailable :: ClientVPNEndpointStatusCode
pattern CVESCAvailable = ClientVPNEndpointStatusCode' "available"

pattern CVESCDeleted :: ClientVPNEndpointStatusCode
pattern CVESCDeleted = ClientVPNEndpointStatusCode' "deleted"

pattern CVESCDeleting :: ClientVPNEndpointStatusCode
pattern CVESCDeleting = ClientVPNEndpointStatusCode' "deleting"

pattern CVESCPendingAssociate :: ClientVPNEndpointStatusCode
pattern CVESCPendingAssociate = ClientVPNEndpointStatusCode' "pending-associate"

{-# COMPLETE
  CVESCAvailable,
  CVESCDeleted,
  CVESCDeleting,
  CVESCPendingAssociate,
  ClientVPNEndpointStatusCode' #-}

instance FromText ClientVPNEndpointStatusCode where
    parser = (ClientVPNEndpointStatusCode' . mk) <$> takeText

instance ToText ClientVPNEndpointStatusCode where
    toText (ClientVPNEndpointStatusCode' ci) = original ci

-- | Represents an enum of /known/ $ClientVPNEndpointStatusCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ClientVPNEndpointStatusCode where
    toEnum i = case i of
        0 -> CVESCAvailable
        1 -> CVESCDeleted
        2 -> CVESCDeleting
        3 -> CVESCPendingAssociate
        _ -> (error . showText) $ "Unknown index for ClientVPNEndpointStatusCode: " <> toText i
    fromEnum x = case x of
        CVESCAvailable -> 0
        CVESCDeleted -> 1
        CVESCDeleting -> 2
        CVESCPendingAssociate -> 3
        ClientVPNEndpointStatusCode' name -> (error . showText) $ "Unknown ClientVPNEndpointStatusCode: " <> original name

-- | Represents the bounds of /known/ $ClientVPNEndpointStatusCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ClientVPNEndpointStatusCode where
    minBound = CVESCAvailable
    maxBound = CVESCPendingAssociate

instance Hashable     ClientVPNEndpointStatusCode
instance NFData       ClientVPNEndpointStatusCode
instance ToByteString ClientVPNEndpointStatusCode
instance ToQuery      ClientVPNEndpointStatusCode
instance ToHeader     ClientVPNEndpointStatusCode

instance FromXML ClientVPNEndpointStatusCode where
    parseXML = parseXMLText "ClientVPNEndpointStatusCode"
