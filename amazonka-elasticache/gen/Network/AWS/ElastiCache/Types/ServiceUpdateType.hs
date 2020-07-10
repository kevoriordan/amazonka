{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ServiceUpdateType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.ServiceUpdateType (
  ServiceUpdateType (
    ..
    , SecurityUpdate
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ServiceUpdateType = ServiceUpdateType' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern SecurityUpdate :: ServiceUpdateType
pattern SecurityUpdate = ServiceUpdateType' "security-update"

{-# COMPLETE
  SecurityUpdate,
  ServiceUpdateType' #-}

instance FromText ServiceUpdateType where
    parser = (ServiceUpdateType' . mk) <$> takeText

instance ToText ServiceUpdateType where
    toText (ServiceUpdateType' ci) = original ci

-- | Represents an enum of /known/ $ServiceUpdateType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ServiceUpdateType where
    toEnum i = case i of
        0 -> SecurityUpdate
        _ -> (error . showText) $ "Unknown index for ServiceUpdateType: " <> toText i
    fromEnum x = case x of
        SecurityUpdate -> 0
        ServiceUpdateType' name -> (error . showText) $ "Unknown ServiceUpdateType: " <> original name

-- | Represents the bounds of /known/ $ServiceUpdateType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ServiceUpdateType where
    minBound = SecurityUpdate
    maxBound = SecurityUpdate

instance Hashable     ServiceUpdateType
instance NFData       ServiceUpdateType
instance ToByteString ServiceUpdateType
instance ToQuery      ServiceUpdateType
instance ToHeader     ServiceUpdateType

instance FromXML ServiceUpdateType where
    parseXML = parseXMLText "ServiceUpdateType"
