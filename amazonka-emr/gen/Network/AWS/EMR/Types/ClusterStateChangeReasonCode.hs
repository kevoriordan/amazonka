{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ClusterStateChangeReasonCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.ClusterStateChangeReasonCode (
  ClusterStateChangeReasonCode (
    ..
    , CSCRCAllStepsCompleted
    , CSCRCBootstrapFailure
    , CSCRCInstanceFailure
    , CSCRCInstanceFleetTimeout
    , CSCRCInternalError
    , CSCRCStepFailure
    , CSCRCUserRequest
    , CSCRCValidationError
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ClusterStateChangeReasonCode = ClusterStateChangeReasonCode' (CI
                                                                     Text)
                                      deriving (Eq, Ord, Read, Show, Data,
                                                Typeable, Generic)

pattern CSCRCAllStepsCompleted :: ClusterStateChangeReasonCode
pattern CSCRCAllStepsCompleted = ClusterStateChangeReasonCode' "ALL_STEPS_COMPLETED"

pattern CSCRCBootstrapFailure :: ClusterStateChangeReasonCode
pattern CSCRCBootstrapFailure = ClusterStateChangeReasonCode' "BOOTSTRAP_FAILURE"

pattern CSCRCInstanceFailure :: ClusterStateChangeReasonCode
pattern CSCRCInstanceFailure = ClusterStateChangeReasonCode' "INSTANCE_FAILURE"

pattern CSCRCInstanceFleetTimeout :: ClusterStateChangeReasonCode
pattern CSCRCInstanceFleetTimeout = ClusterStateChangeReasonCode' "INSTANCE_FLEET_TIMEOUT"

pattern CSCRCInternalError :: ClusterStateChangeReasonCode
pattern CSCRCInternalError = ClusterStateChangeReasonCode' "INTERNAL_ERROR"

pattern CSCRCStepFailure :: ClusterStateChangeReasonCode
pattern CSCRCStepFailure = ClusterStateChangeReasonCode' "STEP_FAILURE"

pattern CSCRCUserRequest :: ClusterStateChangeReasonCode
pattern CSCRCUserRequest = ClusterStateChangeReasonCode' "USER_REQUEST"

pattern CSCRCValidationError :: ClusterStateChangeReasonCode
pattern CSCRCValidationError = ClusterStateChangeReasonCode' "VALIDATION_ERROR"

{-# COMPLETE
  CSCRCAllStepsCompleted,
  CSCRCBootstrapFailure,
  CSCRCInstanceFailure,
  CSCRCInstanceFleetTimeout,
  CSCRCInternalError,
  CSCRCStepFailure,
  CSCRCUserRequest,
  CSCRCValidationError,
  ClusterStateChangeReasonCode' #-}

instance FromText ClusterStateChangeReasonCode where
    parser = (ClusterStateChangeReasonCode' . mk) <$> takeText

instance ToText ClusterStateChangeReasonCode where
    toText (ClusterStateChangeReasonCode' ci) = original ci

-- | Represents an enum of /known/ $ClusterStateChangeReasonCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ClusterStateChangeReasonCode where
    toEnum i = case i of
        0 -> CSCRCAllStepsCompleted
        1 -> CSCRCBootstrapFailure
        2 -> CSCRCInstanceFailure
        3 -> CSCRCInstanceFleetTimeout
        4 -> CSCRCInternalError
        5 -> CSCRCStepFailure
        6 -> CSCRCUserRequest
        7 -> CSCRCValidationError
        _ -> (error . showText) $ "Unknown index for ClusterStateChangeReasonCode: " <> toText i
    fromEnum x = case x of
        CSCRCAllStepsCompleted -> 0
        CSCRCBootstrapFailure -> 1
        CSCRCInstanceFailure -> 2
        CSCRCInstanceFleetTimeout -> 3
        CSCRCInternalError -> 4
        CSCRCStepFailure -> 5
        CSCRCUserRequest -> 6
        CSCRCValidationError -> 7
        ClusterStateChangeReasonCode' name -> (error . showText) $ "Unknown ClusterStateChangeReasonCode: " <> original name

-- | Represents the bounds of /known/ $ClusterStateChangeReasonCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ClusterStateChangeReasonCode where
    minBound = CSCRCAllStepsCompleted
    maxBound = CSCRCValidationError

instance Hashable     ClusterStateChangeReasonCode
instance NFData       ClusterStateChangeReasonCode
instance ToByteString ClusterStateChangeReasonCode
instance ToQuery      ClusterStateChangeReasonCode
instance ToHeader     ClusterStateChangeReasonCode

instance FromJSON ClusterStateChangeReasonCode where
    parseJSON = parseJSONText "ClusterStateChangeReasonCode"
