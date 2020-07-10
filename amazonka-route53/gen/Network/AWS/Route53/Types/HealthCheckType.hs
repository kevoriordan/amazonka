{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.HealthCheckType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types.HealthCheckType (
  HealthCheckType (
    ..
    , Calculated
    , CloudwatchMetric
    , HTTP
    , HTTPS
    , HTTPSStrMatch
    , HTTPStrMatch
    , TCP
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.Route53.Internal

data HealthCheckType = HealthCheckType' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Calculated :: HealthCheckType
pattern Calculated = HealthCheckType' "CALCULATED"

pattern CloudwatchMetric :: HealthCheckType
pattern CloudwatchMetric = HealthCheckType' "CLOUDWATCH_METRIC"

pattern HTTP :: HealthCheckType
pattern HTTP = HealthCheckType' "HTTP"

pattern HTTPS :: HealthCheckType
pattern HTTPS = HealthCheckType' "HTTPS"

pattern HTTPSStrMatch :: HealthCheckType
pattern HTTPSStrMatch = HealthCheckType' "HTTPS_STR_MATCH"

pattern HTTPStrMatch :: HealthCheckType
pattern HTTPStrMatch = HealthCheckType' "HTTP_STR_MATCH"

pattern TCP :: HealthCheckType
pattern TCP = HealthCheckType' "TCP"

{-# COMPLETE
  Calculated,
  CloudwatchMetric,
  HTTP,
  HTTPS,
  HTTPSStrMatch,
  HTTPStrMatch,
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
        0 -> Calculated
        1 -> CloudwatchMetric
        2 -> HTTP
        3 -> HTTPS
        4 -> HTTPSStrMatch
        5 -> HTTPStrMatch
        6 -> TCP
        _ -> (error . showText) $ "Unknown index for HealthCheckType: " <> toText i
    fromEnum x = case x of
        Calculated -> 0
        CloudwatchMetric -> 1
        HTTP -> 2
        HTTPS -> 3
        HTTPSStrMatch -> 4
        HTTPStrMatch -> 5
        TCP -> 6
        HealthCheckType' name -> (error . showText) $ "Unknown HealthCheckType: " <> original name

-- | Represents the bounds of /known/ $HealthCheckType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HealthCheckType where
    minBound = Calculated
    maxBound = TCP

instance Hashable     HealthCheckType
instance NFData       HealthCheckType
instance ToByteString HealthCheckType
instance ToQuery      HealthCheckType
instance ToHeader     HealthCheckType

instance FromXML HealthCheckType where
    parseXML = parseXMLText "HealthCheckType"

instance ToXML HealthCheckType where
    toXML = toXMLText
