{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ServiceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.ServiceType (
  ServiceType (
    ..
    , CredentialProvider
    , Data
    , Jobs
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ServiceType = ServiceType' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern CredentialProvider :: ServiceType
pattern CredentialProvider = ServiceType' "CREDENTIAL_PROVIDER"

pattern Data :: ServiceType
pattern Data = ServiceType' "DATA"

pattern Jobs :: ServiceType
pattern Jobs = ServiceType' "JOBS"

{-# COMPLETE
  CredentialProvider,
  Data,
  Jobs,
  ServiceType' #-}

instance FromText ServiceType where
    parser = (ServiceType' . mk) <$> takeText

instance ToText ServiceType where
    toText (ServiceType' ci) = original ci

-- | Represents an enum of /known/ $ServiceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ServiceType where
    toEnum i = case i of
        0 -> CredentialProvider
        1 -> Data
        2 -> Jobs
        _ -> (error . showText) $ "Unknown index for ServiceType: " <> toText i
    fromEnum x = case x of
        CredentialProvider -> 0
        Data -> 1
        Jobs -> 2
        ServiceType' name -> (error . showText) $ "Unknown ServiceType: " <> original name

-- | Represents the bounds of /known/ $ServiceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ServiceType where
    minBound = CredentialProvider
    maxBound = Jobs

instance Hashable     ServiceType
instance NFData       ServiceType
instance ToByteString ServiceType
instance ToQuery      ServiceType
instance ToHeader     ServiceType

instance ToJSON ServiceType where
    toJSON = toJSONText

instance FromJSON ServiceType where
    parseJSON = parseJSONText "ServiceType"
