{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentHealthStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.EnvironmentHealthStatus (
  EnvironmentHealthStatus (
    ..
    , EHSDegraded
    , EHSInfo
    , EHSNoData
    , EHSOK
    , EHSPending
    , EHSSevere
    , EHSSuspended
    , EHSUnknown
    , EHSWarning
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EnvironmentHealthStatus = EnvironmentHealthStatus' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern EHSDegraded :: EnvironmentHealthStatus
pattern EHSDegraded = EnvironmentHealthStatus' "Degraded"

pattern EHSInfo :: EnvironmentHealthStatus
pattern EHSInfo = EnvironmentHealthStatus' "Info"

pattern EHSNoData :: EnvironmentHealthStatus
pattern EHSNoData = EnvironmentHealthStatus' "NoData"

pattern EHSOK :: EnvironmentHealthStatus
pattern EHSOK = EnvironmentHealthStatus' "Ok"

pattern EHSPending :: EnvironmentHealthStatus
pattern EHSPending = EnvironmentHealthStatus' "Pending"

pattern EHSSevere :: EnvironmentHealthStatus
pattern EHSSevere = EnvironmentHealthStatus' "Severe"

pattern EHSSuspended :: EnvironmentHealthStatus
pattern EHSSuspended = EnvironmentHealthStatus' "Suspended"

pattern EHSUnknown :: EnvironmentHealthStatus
pattern EHSUnknown = EnvironmentHealthStatus' "Unknown"

pattern EHSWarning :: EnvironmentHealthStatus
pattern EHSWarning = EnvironmentHealthStatus' "Warning"

{-# COMPLETE
  EHSDegraded,
  EHSInfo,
  EHSNoData,
  EHSOK,
  EHSPending,
  EHSSevere,
  EHSSuspended,
  EHSUnknown,
  EHSWarning,
  EnvironmentHealthStatus' #-}

instance FromText EnvironmentHealthStatus where
    parser = (EnvironmentHealthStatus' . mk) <$> takeText

instance ToText EnvironmentHealthStatus where
    toText (EnvironmentHealthStatus' ci) = original ci

-- | Represents an enum of /known/ $EnvironmentHealthStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EnvironmentHealthStatus where
    toEnum i = case i of
        0 -> EHSDegraded
        1 -> EHSInfo
        2 -> EHSNoData
        3 -> EHSOK
        4 -> EHSPending
        5 -> EHSSevere
        6 -> EHSSuspended
        7 -> EHSUnknown
        8 -> EHSWarning
        _ -> (error . showText) $ "Unknown index for EnvironmentHealthStatus: " <> toText i
    fromEnum x = case x of
        EHSDegraded -> 0
        EHSInfo -> 1
        EHSNoData -> 2
        EHSOK -> 3
        EHSPending -> 4
        EHSSevere -> 5
        EHSSuspended -> 6
        EHSUnknown -> 7
        EHSWarning -> 8
        EnvironmentHealthStatus' name -> (error . showText) $ "Unknown EnvironmentHealthStatus: " <> original name

-- | Represents the bounds of /known/ $EnvironmentHealthStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EnvironmentHealthStatus where
    minBound = EHSDegraded
    maxBound = EHSWarning

instance Hashable     EnvironmentHealthStatus
instance NFData       EnvironmentHealthStatus
instance ToByteString EnvironmentHealthStatus
instance ToQuery      EnvironmentHealthStatus
instance ToHeader     EnvironmentHealthStatus

instance FromXML EnvironmentHealthStatus where
    parseXML = parseXMLText "EnvironmentHealthStatus"
