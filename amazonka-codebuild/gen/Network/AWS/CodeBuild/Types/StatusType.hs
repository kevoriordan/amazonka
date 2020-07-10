{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.StatusType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.StatusType (
  StatusType (
    ..
    , STFailed
    , STFault
    , STInProgress
    , STStopped
    , STSucceeded
    , STTimedOut
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StatusType = StatusType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern STFailed :: StatusType
pattern STFailed = StatusType' "FAILED"

pattern STFault :: StatusType
pattern STFault = StatusType' "FAULT"

pattern STInProgress :: StatusType
pattern STInProgress = StatusType' "IN_PROGRESS"

pattern STStopped :: StatusType
pattern STStopped = StatusType' "STOPPED"

pattern STSucceeded :: StatusType
pattern STSucceeded = StatusType' "SUCCEEDED"

pattern STTimedOut :: StatusType
pattern STTimedOut = StatusType' "TIMED_OUT"

{-# COMPLETE
  STFailed,
  STFault,
  STInProgress,
  STStopped,
  STSucceeded,
  STTimedOut,
  StatusType' #-}

instance FromText StatusType where
    parser = (StatusType' . mk) <$> takeText

instance ToText StatusType where
    toText (StatusType' ci) = original ci

-- | Represents an enum of /known/ $StatusType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StatusType where
    toEnum i = case i of
        0 -> STFailed
        1 -> STFault
        2 -> STInProgress
        3 -> STStopped
        4 -> STSucceeded
        5 -> STTimedOut
        _ -> (error . showText) $ "Unknown index for StatusType: " <> toText i
    fromEnum x = case x of
        STFailed -> 0
        STFault -> 1
        STInProgress -> 2
        STStopped -> 3
        STSucceeded -> 4
        STTimedOut -> 5
        StatusType' name -> (error . showText) $ "Unknown StatusType: " <> original name

-- | Represents the bounds of /known/ $StatusType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StatusType where
    minBound = STFailed
    maxBound = STTimedOut

instance Hashable     StatusType
instance NFData       StatusType
instance ToByteString StatusType
instance ToQuery      StatusType
instance ToHeader     StatusType

instance FromJSON StatusType where
    parseJSON = parseJSONText "StatusType"
