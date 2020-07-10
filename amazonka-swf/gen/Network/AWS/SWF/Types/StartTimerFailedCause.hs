{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.StartTimerFailedCause
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.StartTimerFailedCause (
  StartTimerFailedCause (
    ..
    , STFCOpenTimersLimitExceeded
    , STFCOperationNotPermitted
    , STFCTimerCreationRateExceeded
    , STFCTimerIdAlreadyInUse
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StartTimerFailedCause = StartTimerFailedCause' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern STFCOpenTimersLimitExceeded :: StartTimerFailedCause
pattern STFCOpenTimersLimitExceeded = StartTimerFailedCause' "OPEN_TIMERS_LIMIT_EXCEEDED"

pattern STFCOperationNotPermitted :: StartTimerFailedCause
pattern STFCOperationNotPermitted = StartTimerFailedCause' "OPERATION_NOT_PERMITTED"

pattern STFCTimerCreationRateExceeded :: StartTimerFailedCause
pattern STFCTimerCreationRateExceeded = StartTimerFailedCause' "TIMER_CREATION_RATE_EXCEEDED"

pattern STFCTimerIdAlreadyInUse :: StartTimerFailedCause
pattern STFCTimerIdAlreadyInUse = StartTimerFailedCause' "TIMER_ID_ALREADY_IN_USE"

{-# COMPLETE
  STFCOpenTimersLimitExceeded,
  STFCOperationNotPermitted,
  STFCTimerCreationRateExceeded,
  STFCTimerIdAlreadyInUse,
  StartTimerFailedCause' #-}

instance FromText StartTimerFailedCause where
    parser = (StartTimerFailedCause' . mk) <$> takeText

instance ToText StartTimerFailedCause where
    toText (StartTimerFailedCause' ci) = original ci

-- | Represents an enum of /known/ $StartTimerFailedCause.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StartTimerFailedCause where
    toEnum i = case i of
        0 -> STFCOpenTimersLimitExceeded
        1 -> STFCOperationNotPermitted
        2 -> STFCTimerCreationRateExceeded
        3 -> STFCTimerIdAlreadyInUse
        _ -> (error . showText) $ "Unknown index for StartTimerFailedCause: " <> toText i
    fromEnum x = case x of
        STFCOpenTimersLimitExceeded -> 0
        STFCOperationNotPermitted -> 1
        STFCTimerCreationRateExceeded -> 2
        STFCTimerIdAlreadyInUse -> 3
        StartTimerFailedCause' name -> (error . showText) $ "Unknown StartTimerFailedCause: " <> original name

-- | Represents the bounds of /known/ $StartTimerFailedCause.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StartTimerFailedCause where
    minBound = STFCOpenTimersLimitExceeded
    maxBound = STFCTimerIdAlreadyInUse

instance Hashable     StartTimerFailedCause
instance NFData       StartTimerFailedCause
instance ToByteString StartTimerFailedCause
instance ToQuery      StartTimerFailedCause
instance ToHeader     StartTimerFailedCause

instance FromJSON StartTimerFailedCause where
    parseJSON = parseJSONText "StartTimerFailedCause"
