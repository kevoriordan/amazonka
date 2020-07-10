{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.TargetHealthReasonEnum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.TargetHealthReasonEnum (
  TargetHealthReasonEnum (
    ..
    , Elb_InitialHealthChecking
    , Elb_InternalError
    , Elb_RegistrationInProgress
    , Target_DeregistrationInProgress
    , Target_FailedHealthChecks
    , Target_HealthCheckDisabled
    , Target_IPUnusable
    , Target_InvalidState
    , Target_NotInUse
    , Target_NotRegistered
    , Target_ResponseCodeMismatch
    , Target_Timeout
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TargetHealthReasonEnum = TargetHealthReasonEnum' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern Elb_InitialHealthChecking :: TargetHealthReasonEnum
pattern Elb_InitialHealthChecking = TargetHealthReasonEnum' "Elb.InitialHealthChecking"

pattern Elb_InternalError :: TargetHealthReasonEnum
pattern Elb_InternalError = TargetHealthReasonEnum' "Elb.InternalError"

pattern Elb_RegistrationInProgress :: TargetHealthReasonEnum
pattern Elb_RegistrationInProgress = TargetHealthReasonEnum' "Elb.RegistrationInProgress"

pattern Target_DeregistrationInProgress :: TargetHealthReasonEnum
pattern Target_DeregistrationInProgress = TargetHealthReasonEnum' "Target.DeregistrationInProgress"

pattern Target_FailedHealthChecks :: TargetHealthReasonEnum
pattern Target_FailedHealthChecks = TargetHealthReasonEnum' "Target.FailedHealthChecks"

pattern Target_HealthCheckDisabled :: TargetHealthReasonEnum
pattern Target_HealthCheckDisabled = TargetHealthReasonEnum' "Target.HealthCheckDisabled"

pattern Target_IPUnusable :: TargetHealthReasonEnum
pattern Target_IPUnusable = TargetHealthReasonEnum' "Target.IpUnusable"

pattern Target_InvalidState :: TargetHealthReasonEnum
pattern Target_InvalidState = TargetHealthReasonEnum' "Target.InvalidState"

pattern Target_NotInUse :: TargetHealthReasonEnum
pattern Target_NotInUse = TargetHealthReasonEnum' "Target.NotInUse"

pattern Target_NotRegistered :: TargetHealthReasonEnum
pattern Target_NotRegistered = TargetHealthReasonEnum' "Target.NotRegistered"

pattern Target_ResponseCodeMismatch :: TargetHealthReasonEnum
pattern Target_ResponseCodeMismatch = TargetHealthReasonEnum' "Target.ResponseCodeMismatch"

pattern Target_Timeout :: TargetHealthReasonEnum
pattern Target_Timeout = TargetHealthReasonEnum' "Target.Timeout"

{-# COMPLETE
  Elb_InitialHealthChecking,
  Elb_InternalError,
  Elb_RegistrationInProgress,
  Target_DeregistrationInProgress,
  Target_FailedHealthChecks,
  Target_HealthCheckDisabled,
  Target_IPUnusable,
  Target_InvalidState,
  Target_NotInUse,
  Target_NotRegistered,
  Target_ResponseCodeMismatch,
  Target_Timeout,
  TargetHealthReasonEnum' #-}

instance FromText TargetHealthReasonEnum where
    parser = (TargetHealthReasonEnum' . mk) <$> takeText

instance ToText TargetHealthReasonEnum where
    toText (TargetHealthReasonEnum' ci) = original ci

-- | Represents an enum of /known/ $TargetHealthReasonEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TargetHealthReasonEnum where
    toEnum i = case i of
        0 -> Elb_InitialHealthChecking
        1 -> Elb_InternalError
        2 -> Elb_RegistrationInProgress
        3 -> Target_DeregistrationInProgress
        4 -> Target_FailedHealthChecks
        5 -> Target_HealthCheckDisabled
        6 -> Target_IPUnusable
        7 -> Target_InvalidState
        8 -> Target_NotInUse
        9 -> Target_NotRegistered
        10 -> Target_ResponseCodeMismatch
        11 -> Target_Timeout
        _ -> (error . showText) $ "Unknown index for TargetHealthReasonEnum: " <> toText i
    fromEnum x = case x of
        Elb_InitialHealthChecking -> 0
        Elb_InternalError -> 1
        Elb_RegistrationInProgress -> 2
        Target_DeregistrationInProgress -> 3
        Target_FailedHealthChecks -> 4
        Target_HealthCheckDisabled -> 5
        Target_IPUnusable -> 6
        Target_InvalidState -> 7
        Target_NotInUse -> 8
        Target_NotRegistered -> 9
        Target_ResponseCodeMismatch -> 10
        Target_Timeout -> 11
        TargetHealthReasonEnum' name -> (error . showText) $ "Unknown TargetHealthReasonEnum: " <> original name

-- | Represents the bounds of /known/ $TargetHealthReasonEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TargetHealthReasonEnum where
    minBound = Elb_InitialHealthChecking
    maxBound = Target_Timeout

instance Hashable     TargetHealthReasonEnum
instance NFData       TargetHealthReasonEnum
instance ToByteString TargetHealthReasonEnum
instance ToQuery      TargetHealthReasonEnum
instance ToHeader     TargetHealthReasonEnum

instance FromXML TargetHealthReasonEnum where
    parseXML = parseXMLText "TargetHealthReasonEnum"
