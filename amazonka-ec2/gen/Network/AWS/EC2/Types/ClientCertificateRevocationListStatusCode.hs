{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientCertificateRevocationListStatusCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ClientCertificateRevocationListStatusCode (
  ClientCertificateRevocationListStatusCode (
    ..
    , CCRLSCActive
    , CCRLSCPending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ClientCertificateRevocationListStatusCode = ClientCertificateRevocationListStatusCode' (CI
                                                                                               Text)
                                                   deriving (Eq, Ord, Read,
                                                             Show, Data,
                                                             Typeable, Generic)

pattern CCRLSCActive :: ClientCertificateRevocationListStatusCode
pattern CCRLSCActive = ClientCertificateRevocationListStatusCode' "active"

pattern CCRLSCPending :: ClientCertificateRevocationListStatusCode
pattern CCRLSCPending = ClientCertificateRevocationListStatusCode' "pending"

{-# COMPLETE
  CCRLSCActive,
  CCRLSCPending,
  ClientCertificateRevocationListStatusCode' #-}

instance FromText ClientCertificateRevocationListStatusCode where
    parser = (ClientCertificateRevocationListStatusCode' . mk) <$> takeText

instance ToText ClientCertificateRevocationListStatusCode where
    toText (ClientCertificateRevocationListStatusCode' ci) = original ci

-- | Represents an enum of /known/ $ClientCertificateRevocationListStatusCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ClientCertificateRevocationListStatusCode where
    toEnum i = case i of
        0 -> CCRLSCActive
        1 -> CCRLSCPending
        _ -> (error . showText) $ "Unknown index for ClientCertificateRevocationListStatusCode: " <> toText i
    fromEnum x = case x of
        CCRLSCActive -> 0
        CCRLSCPending -> 1
        ClientCertificateRevocationListStatusCode' name -> (error . showText) $ "Unknown ClientCertificateRevocationListStatusCode: " <> original name

-- | Represents the bounds of /known/ $ClientCertificateRevocationListStatusCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ClientCertificateRevocationListStatusCode where
    minBound = CCRLSCActive
    maxBound = CCRLSCPending

instance Hashable     ClientCertificateRevocationListStatusCode
instance NFData       ClientCertificateRevocationListStatusCode
instance ToByteString ClientCertificateRevocationListStatusCode
instance ToQuery      ClientCertificateRevocationListStatusCode
instance ToHeader     ClientCertificateRevocationListStatusCode

instance FromXML ClientCertificateRevocationListStatusCode where
    parseXML = parseXMLText "ClientCertificateRevocationListStatusCode"
