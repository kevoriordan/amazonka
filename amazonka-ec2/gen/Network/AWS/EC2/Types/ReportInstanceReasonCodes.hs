{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReportInstanceReasonCodes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ReportInstanceReasonCodes (
  ReportInstanceReasonCodes (
    ..
    , InstanceStuckInState
    , NotAcceptingCredentials
    , Other
    , PasswordNotAvailable
    , PerformanceEBSVolume
    , PerformanceInstanceStore
    , PerformanceNetwork
    , PerformanceOther
    , Unresponsive
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ReportInstanceReasonCodes = ReportInstanceReasonCodes' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern InstanceStuckInState :: ReportInstanceReasonCodes
pattern InstanceStuckInState = ReportInstanceReasonCodes' "instance-stuck-in-state"

pattern NotAcceptingCredentials :: ReportInstanceReasonCodes
pattern NotAcceptingCredentials = ReportInstanceReasonCodes' "not-accepting-credentials"

pattern Other :: ReportInstanceReasonCodes
pattern Other = ReportInstanceReasonCodes' "other"

pattern PasswordNotAvailable :: ReportInstanceReasonCodes
pattern PasswordNotAvailable = ReportInstanceReasonCodes' "password-not-available"

pattern PerformanceEBSVolume :: ReportInstanceReasonCodes
pattern PerformanceEBSVolume = ReportInstanceReasonCodes' "performance-ebs-volume"

pattern PerformanceInstanceStore :: ReportInstanceReasonCodes
pattern PerformanceInstanceStore = ReportInstanceReasonCodes' "performance-instance-store"

pattern PerformanceNetwork :: ReportInstanceReasonCodes
pattern PerformanceNetwork = ReportInstanceReasonCodes' "performance-network"

pattern PerformanceOther :: ReportInstanceReasonCodes
pattern PerformanceOther = ReportInstanceReasonCodes' "performance-other"

pattern Unresponsive :: ReportInstanceReasonCodes
pattern Unresponsive = ReportInstanceReasonCodes' "unresponsive"

{-# COMPLETE
  InstanceStuckInState,
  NotAcceptingCredentials,
  Other,
  PasswordNotAvailable,
  PerformanceEBSVolume,
  PerformanceInstanceStore,
  PerformanceNetwork,
  PerformanceOther,
  Unresponsive,
  ReportInstanceReasonCodes' #-}

instance FromText ReportInstanceReasonCodes where
    parser = (ReportInstanceReasonCodes' . mk) <$> takeText

instance ToText ReportInstanceReasonCodes where
    toText (ReportInstanceReasonCodes' ci) = original ci

-- | Represents an enum of /known/ $ReportInstanceReasonCodes.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReportInstanceReasonCodes where
    toEnum i = case i of
        0 -> InstanceStuckInState
        1 -> NotAcceptingCredentials
        2 -> Other
        3 -> PasswordNotAvailable
        4 -> PerformanceEBSVolume
        5 -> PerformanceInstanceStore
        6 -> PerformanceNetwork
        7 -> PerformanceOther
        8 -> Unresponsive
        _ -> (error . showText) $ "Unknown index for ReportInstanceReasonCodes: " <> toText i
    fromEnum x = case x of
        InstanceStuckInState -> 0
        NotAcceptingCredentials -> 1
        Other -> 2
        PasswordNotAvailable -> 3
        PerformanceEBSVolume -> 4
        PerformanceInstanceStore -> 5
        PerformanceNetwork -> 6
        PerformanceOther -> 7
        Unresponsive -> 8
        ReportInstanceReasonCodes' name -> (error . showText) $ "Unknown ReportInstanceReasonCodes: " <> original name

-- | Represents the bounds of /known/ $ReportInstanceReasonCodes.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReportInstanceReasonCodes where
    minBound = InstanceStuckInState
    maxBound = Unresponsive

instance Hashable     ReportInstanceReasonCodes
instance NFData       ReportInstanceReasonCodes
instance ToByteString ReportInstanceReasonCodes
instance ToQuery      ReportInstanceReasonCodes
instance ToHeader     ReportInstanceReasonCodes
