{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.OperationStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.OperationStatus (
  OperationStatus (
    ..
    , OSCompleted
    , OSFailed
    , OSNotStarted
    , OSStarted
    , OSSucceeded
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data OperationStatus = OperationStatus' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern OSCompleted :: OperationStatus
pattern OSCompleted = OperationStatus' "Completed"

pattern OSFailed :: OperationStatus
pattern OSFailed = OperationStatus' "Failed"

pattern OSNotStarted :: OperationStatus
pattern OSNotStarted = OperationStatus' "NotStarted"

pattern OSStarted :: OperationStatus
pattern OSStarted = OperationStatus' "Started"

pattern OSSucceeded :: OperationStatus
pattern OSSucceeded = OperationStatus' "Succeeded"

{-# COMPLETE
  OSCompleted,
  OSFailed,
  OSNotStarted,
  OSStarted,
  OSSucceeded,
  OperationStatus' #-}

instance FromText OperationStatus where
    parser = (OperationStatus' . mk) <$> takeText

instance ToText OperationStatus where
    toText (OperationStatus' ci) = original ci

-- | Represents an enum of /known/ $OperationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OperationStatus where
    toEnum i = case i of
        0 -> OSCompleted
        1 -> OSFailed
        2 -> OSNotStarted
        3 -> OSStarted
        4 -> OSSucceeded
        _ -> (error . showText) $ "Unknown index for OperationStatus: " <> toText i
    fromEnum x = case x of
        OSCompleted -> 0
        OSFailed -> 1
        OSNotStarted -> 2
        OSStarted -> 3
        OSSucceeded -> 4
        OperationStatus' name -> (error . showText) $ "Unknown OperationStatus: " <> original name

-- | Represents the bounds of /known/ $OperationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OperationStatus where
    minBound = OSCompleted
    maxBound = OSSucceeded

instance Hashable     OperationStatus
instance NFData       OperationStatus
instance ToByteString OperationStatus
instance ToQuery      OperationStatus
instance ToHeader     OperationStatus

instance FromJSON OperationStatus where
    parseJSON = parseJSONText "OperationStatus"
