{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVPNAuthenticationType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ClientVPNAuthenticationType (
  ClientVPNAuthenticationType (
    ..
    , CertificateAuthentication
    , DirectoryServiceAuthentication
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ClientVPNAuthenticationType = ClientVPNAuthenticationType' (CI
                                                                   Text)
                                     deriving (Eq, Ord, Read, Show, Data,
                                               Typeable, Generic)

pattern CertificateAuthentication :: ClientVPNAuthenticationType
pattern CertificateAuthentication = ClientVPNAuthenticationType' "certificate-authentication"

pattern DirectoryServiceAuthentication :: ClientVPNAuthenticationType
pattern DirectoryServiceAuthentication = ClientVPNAuthenticationType' "directory-service-authentication"

{-# COMPLETE
  CertificateAuthentication,
  DirectoryServiceAuthentication,
  ClientVPNAuthenticationType' #-}

instance FromText ClientVPNAuthenticationType where
    parser = (ClientVPNAuthenticationType' . mk) <$> takeText

instance ToText ClientVPNAuthenticationType where
    toText (ClientVPNAuthenticationType' ci) = original ci

-- | Represents an enum of /known/ $ClientVPNAuthenticationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ClientVPNAuthenticationType where
    toEnum i = case i of
        0 -> CertificateAuthentication
        1 -> DirectoryServiceAuthentication
        _ -> (error . showText) $ "Unknown index for ClientVPNAuthenticationType: " <> toText i
    fromEnum x = case x of
        CertificateAuthentication -> 0
        DirectoryServiceAuthentication -> 1
        ClientVPNAuthenticationType' name -> (error . showText) $ "Unknown ClientVPNAuthenticationType: " <> original name

-- | Represents the bounds of /known/ $ClientVPNAuthenticationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ClientVPNAuthenticationType where
    minBound = CertificateAuthentication
    maxBound = DirectoryServiceAuthentication

instance Hashable     ClientVPNAuthenticationType
instance NFData       ClientVPNAuthenticationType
instance ToByteString ClientVPNAuthenticationType
instance ToQuery      ClientVPNAuthenticationType
instance ToHeader     ClientVPNAuthenticationType

instance FromXML ClientVPNAuthenticationType where
    parseXML = parseXMLText "ClientVPNAuthenticationType"
