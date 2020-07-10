{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.StageExecutionStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.StageExecutionStatus (
  StageExecutionStatus (
    ..
    , SESFailed
    , SESInProgress
    , SESStopped
    , SESStopping
    , SESSucceeded
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StageExecutionStatus = StageExecutionStatus' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern SESFailed :: StageExecutionStatus
pattern SESFailed = StageExecutionStatus' "Failed"

pattern SESInProgress :: StageExecutionStatus
pattern SESInProgress = StageExecutionStatus' "InProgress"

pattern SESStopped :: StageExecutionStatus
pattern SESStopped = StageExecutionStatus' "Stopped"

pattern SESStopping :: StageExecutionStatus
pattern SESStopping = StageExecutionStatus' "Stopping"

pattern SESSucceeded :: StageExecutionStatus
pattern SESSucceeded = StageExecutionStatus' "Succeeded"

{-# COMPLETE
  SESFailed,
  SESInProgress,
  SESStopped,
  SESStopping,
  SESSucceeded,
  StageExecutionStatus' #-}

instance FromText StageExecutionStatus where
    parser = (StageExecutionStatus' . mk) <$> takeText

instance ToText StageExecutionStatus where
    toText (StageExecutionStatus' ci) = original ci

-- | Represents an enum of /known/ $StageExecutionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StageExecutionStatus where
    toEnum i = case i of
        0 -> SESFailed
        1 -> SESInProgress
        2 -> SESStopped
        3 -> SESStopping
        4 -> SESSucceeded
        _ -> (error . showText) $ "Unknown index for StageExecutionStatus: " <> toText i
    fromEnum x = case x of
        SESFailed -> 0
        SESInProgress -> 1
        SESStopped -> 2
        SESStopping -> 3
        SESSucceeded -> 4
        StageExecutionStatus' name -> (error . showText) $ "Unknown StageExecutionStatus: " <> original name

-- | Represents the bounds of /known/ $StageExecutionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StageExecutionStatus where
    minBound = SESFailed
    maxBound = SESSucceeded

instance Hashable     StageExecutionStatus
instance NFData       StageExecutionStatus
instance ToByteString StageExecutionStatus
instance ToQuery      StageExecutionStatus
instance ToHeader     StageExecutionStatus

instance FromJSON StageExecutionStatus where
    parseJSON = parseJSONText "StageExecutionStatus"
