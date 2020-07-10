{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.CloseStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.CloseStatus (
  CloseStatus (
    ..
    , Canceled
    , Completed
    , ContinuedAsNew
    , Failed
    , Terminated
    , TimedOut
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CloseStatus = CloseStatus' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Canceled :: CloseStatus
pattern Canceled = CloseStatus' "CANCELED"

pattern Completed :: CloseStatus
pattern Completed = CloseStatus' "COMPLETED"

pattern ContinuedAsNew :: CloseStatus
pattern ContinuedAsNew = CloseStatus' "CONTINUED_AS_NEW"

pattern Failed :: CloseStatus
pattern Failed = CloseStatus' "FAILED"

pattern Terminated :: CloseStatus
pattern Terminated = CloseStatus' "TERMINATED"

pattern TimedOut :: CloseStatus
pattern TimedOut = CloseStatus' "TIMED_OUT"

{-# COMPLETE
  Canceled,
  Completed,
  ContinuedAsNew,
  Failed,
  Terminated,
  TimedOut,
  CloseStatus' #-}

instance FromText CloseStatus where
    parser = (CloseStatus' . mk) <$> takeText

instance ToText CloseStatus where
    toText (CloseStatus' ci) = original ci

-- | Represents an enum of /known/ $CloseStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CloseStatus where
    toEnum i = case i of
        0 -> Canceled
        1 -> Completed
        2 -> ContinuedAsNew
        3 -> Failed
        4 -> Terminated
        5 -> TimedOut
        _ -> (error . showText) $ "Unknown index for CloseStatus: " <> toText i
    fromEnum x = case x of
        Canceled -> 0
        Completed -> 1
        ContinuedAsNew -> 2
        Failed -> 3
        Terminated -> 4
        TimedOut -> 5
        CloseStatus' name -> (error . showText) $ "Unknown CloseStatus: " <> original name

-- | Represents the bounds of /known/ $CloseStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CloseStatus where
    minBound = Canceled
    maxBound = TimedOut

instance Hashable     CloseStatus
instance NFData       CloseStatus
instance ToByteString CloseStatus
instance ToQuery      CloseStatus
instance ToHeader     CloseStatus

instance ToJSON CloseStatus where
    toJSON = toJSONText

instance FromJSON CloseStatus where
    parseJSON = parseJSONText "CloseStatus"
