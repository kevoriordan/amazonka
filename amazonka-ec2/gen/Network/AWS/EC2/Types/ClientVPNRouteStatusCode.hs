{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVPNRouteStatusCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ClientVPNRouteStatusCode (
  ClientVPNRouteStatusCode (
    ..
    , CVRSCActive
    , CVRSCCreating
    , CVRSCDeleting
    , CVRSCFailed
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ClientVPNRouteStatusCode = ClientVPNRouteStatusCode' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern CVRSCActive :: ClientVPNRouteStatusCode
pattern CVRSCActive = ClientVPNRouteStatusCode' "active"

pattern CVRSCCreating :: ClientVPNRouteStatusCode
pattern CVRSCCreating = ClientVPNRouteStatusCode' "creating"

pattern CVRSCDeleting :: ClientVPNRouteStatusCode
pattern CVRSCDeleting = ClientVPNRouteStatusCode' "deleting"

pattern CVRSCFailed :: ClientVPNRouteStatusCode
pattern CVRSCFailed = ClientVPNRouteStatusCode' "failed"

{-# COMPLETE
  CVRSCActive,
  CVRSCCreating,
  CVRSCDeleting,
  CVRSCFailed,
  ClientVPNRouteStatusCode' #-}

instance FromText ClientVPNRouteStatusCode where
    parser = (ClientVPNRouteStatusCode' . mk) <$> takeText

instance ToText ClientVPNRouteStatusCode where
    toText (ClientVPNRouteStatusCode' ci) = original ci

-- | Represents an enum of /known/ $ClientVPNRouteStatusCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ClientVPNRouteStatusCode where
    toEnum i = case i of
        0 -> CVRSCActive
        1 -> CVRSCCreating
        2 -> CVRSCDeleting
        3 -> CVRSCFailed
        _ -> (error . showText) $ "Unknown index for ClientVPNRouteStatusCode: " <> toText i
    fromEnum x = case x of
        CVRSCActive -> 0
        CVRSCCreating -> 1
        CVRSCDeleting -> 2
        CVRSCFailed -> 3
        ClientVPNRouteStatusCode' name -> (error . showText) $ "Unknown ClientVPNRouteStatusCode: " <> original name

-- | Represents the bounds of /known/ $ClientVPNRouteStatusCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ClientVPNRouteStatusCode where
    minBound = CVRSCActive
    maxBound = CVRSCFailed

instance Hashable     ClientVPNRouteStatusCode
instance NFData       ClientVPNRouteStatusCode
instance ToByteString ClientVPNRouteStatusCode
instance ToQuery      ClientVPNRouteStatusCode
instance ToHeader     ClientVPNRouteStatusCode

instance FromXML ClientVPNRouteStatusCode where
    parseXML = parseXMLText "ClientVPNRouteStatusCode"
