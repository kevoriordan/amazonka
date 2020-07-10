{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.HealthCheckType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53AutoNaming.Types.HealthCheckType (
  HealthCheckType (
    ..
    , HTTP
    , HTTPS
    , TCP
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data HealthCheckType = HealthCheckType' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern HTTP :: HealthCheckType
pattern HTTP = HealthCheckType' "HTTP"

pattern HTTPS :: HealthCheckType
pattern HTTPS = HealthCheckType' "HTTPS"

pattern TCP :: HealthCheckType
pattern TCP = HealthCheckType' "TCP"

{-# COMPLETE
  HTTP,
  HTTPS,
  TCP,
  HealthCheckType' #-}

instance FromText HealthCheckType where
    parser = (HealthCheckType' . mk) <$> takeText

instance ToText HealthCheckType where
    toText (HealthCheckType' ci) = original ci

-- | Represents an enum of /known/ $HealthCheckType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HealthCheckType where
    toEnum i = case i of
        0 -> HTTP
        1 -> HTTPS
        2 -> TCP
        _ -> (error . showText) $ "Unknown index for HealthCheckType: " <> toText i
    fromEnum x = case x of
        HTTP -> 0
        HTTPS -> 1
        TCP -> 2
        HealthCheckType' name -> (error . showText) $ "Unknown HealthCheckType: " <> original name

-- | Represents the bounds of /known/ $HealthCheckType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HealthCheckType where
    minBound = HTTP
    maxBound = TCP

instance Hashable     HealthCheckType
instance NFData       HealthCheckType
instance ToByteString HealthCheckType
instance ToQuery      HealthCheckType
instance ToHeader     HealthCheckType

instance ToJSON HealthCheckType where
    toJSON = toJSONText

instance FromJSON HealthCheckType where
    parseJSON = parseJSONText "HealthCheckType"
