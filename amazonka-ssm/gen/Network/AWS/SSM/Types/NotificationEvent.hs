{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.NotificationEvent
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.NotificationEvent (
  NotificationEvent (
    ..
    , NEAll
    , NECancelled
    , NEFailed
    , NEInProgress
    , NESuccess
    , NETimedOut
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data NotificationEvent = NotificationEvent' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern NEAll :: NotificationEvent
pattern NEAll = NotificationEvent' "All"

pattern NECancelled :: NotificationEvent
pattern NECancelled = NotificationEvent' "Cancelled"

pattern NEFailed :: NotificationEvent
pattern NEFailed = NotificationEvent' "Failed"

pattern NEInProgress :: NotificationEvent
pattern NEInProgress = NotificationEvent' "InProgress"

pattern NESuccess :: NotificationEvent
pattern NESuccess = NotificationEvent' "Success"

pattern NETimedOut :: NotificationEvent
pattern NETimedOut = NotificationEvent' "TimedOut"

{-# COMPLETE
  NEAll,
  NECancelled,
  NEFailed,
  NEInProgress,
  NESuccess,
  NETimedOut,
  NotificationEvent' #-}

instance FromText NotificationEvent where
    parser = (NotificationEvent' . mk) <$> takeText

instance ToText NotificationEvent where
    toText (NotificationEvent' ci) = original ci

-- | Represents an enum of /known/ $NotificationEvent.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum NotificationEvent where
    toEnum i = case i of
        0 -> NEAll
        1 -> NECancelled
        2 -> NEFailed
        3 -> NEInProgress
        4 -> NESuccess
        5 -> NETimedOut
        _ -> (error . showText) $ "Unknown index for NotificationEvent: " <> toText i
    fromEnum x = case x of
        NEAll -> 0
        NECancelled -> 1
        NEFailed -> 2
        NEInProgress -> 3
        NESuccess -> 4
        NETimedOut -> 5
        NotificationEvent' name -> (error . showText) $ "Unknown NotificationEvent: " <> original name

-- | Represents the bounds of /known/ $NotificationEvent.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded NotificationEvent where
    minBound = NEAll
    maxBound = NETimedOut

instance Hashable     NotificationEvent
instance NFData       NotificationEvent
instance ToByteString NotificationEvent
instance ToQuery      NotificationEvent
instance ToHeader     NotificationEvent

instance ToJSON NotificationEvent where
    toJSON = toJSONText

instance FromJSON NotificationEvent where
    parseJSON = parseJSONText "NotificationEvent"
