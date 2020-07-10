{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.SessionStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.SessionStatus (
  SessionStatus (
    ..
    , SSConnected
    , SSConnecting
    , SSDisconnected
    , SSFailed
    , SSTerminated
    , SSTerminating
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SessionStatus = SessionStatus' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern SSConnected :: SessionStatus
pattern SSConnected = SessionStatus' "Connected"

pattern SSConnecting :: SessionStatus
pattern SSConnecting = SessionStatus' "Connecting"

pattern SSDisconnected :: SessionStatus
pattern SSDisconnected = SessionStatus' "Disconnected"

pattern SSFailed :: SessionStatus
pattern SSFailed = SessionStatus' "Failed"

pattern SSTerminated :: SessionStatus
pattern SSTerminated = SessionStatus' "Terminated"

pattern SSTerminating :: SessionStatus
pattern SSTerminating = SessionStatus' "Terminating"

{-# COMPLETE
  SSConnected,
  SSConnecting,
  SSDisconnected,
  SSFailed,
  SSTerminated,
  SSTerminating,
  SessionStatus' #-}

instance FromText SessionStatus where
    parser = (SessionStatus' . mk) <$> takeText

instance ToText SessionStatus where
    toText (SessionStatus' ci) = original ci

-- | Represents an enum of /known/ $SessionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SessionStatus where
    toEnum i = case i of
        0 -> SSConnected
        1 -> SSConnecting
        2 -> SSDisconnected
        3 -> SSFailed
        4 -> SSTerminated
        5 -> SSTerminating
        _ -> (error . showText) $ "Unknown index for SessionStatus: " <> toText i
    fromEnum x = case x of
        SSConnected -> 0
        SSConnecting -> 1
        SSDisconnected -> 2
        SSFailed -> 3
        SSTerminated -> 4
        SSTerminating -> 5
        SessionStatus' name -> (error . showText) $ "Unknown SessionStatus: " <> original name

-- | Represents the bounds of /known/ $SessionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SessionStatus where
    minBound = SSConnected
    maxBound = SSTerminating

instance Hashable     SessionStatus
instance NFData       SessionStatus
instance ToByteString SessionStatus
instance ToQuery      SessionStatus
instance ToHeader     SessionStatus

instance FromJSON SessionStatus where
    parseJSON = parseJSONText "SessionStatus"
