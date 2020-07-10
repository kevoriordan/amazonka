{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetOperationStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.StackSetOperationStatus (
  StackSetOperationStatus (
    ..
    , SSOSFailed
    , SSOSQueued
    , SSOSRunning
    , SSOSStopped
    , SSOSStopping
    , SSOSSucceeded
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StackSetOperationStatus = StackSetOperationStatus' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern SSOSFailed :: StackSetOperationStatus
pattern SSOSFailed = StackSetOperationStatus' "FAILED"

pattern SSOSQueued :: StackSetOperationStatus
pattern SSOSQueued = StackSetOperationStatus' "QUEUED"

pattern SSOSRunning :: StackSetOperationStatus
pattern SSOSRunning = StackSetOperationStatus' "RUNNING"

pattern SSOSStopped :: StackSetOperationStatus
pattern SSOSStopped = StackSetOperationStatus' "STOPPED"

pattern SSOSStopping :: StackSetOperationStatus
pattern SSOSStopping = StackSetOperationStatus' "STOPPING"

pattern SSOSSucceeded :: StackSetOperationStatus
pattern SSOSSucceeded = StackSetOperationStatus' "SUCCEEDED"

{-# COMPLETE
  SSOSFailed,
  SSOSQueued,
  SSOSRunning,
  SSOSStopped,
  SSOSStopping,
  SSOSSucceeded,
  StackSetOperationStatus' #-}

instance FromText StackSetOperationStatus where
    parser = (StackSetOperationStatus' . mk) <$> takeText

instance ToText StackSetOperationStatus where
    toText (StackSetOperationStatus' ci) = original ci

-- | Represents an enum of /known/ $StackSetOperationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StackSetOperationStatus where
    toEnum i = case i of
        0 -> SSOSFailed
        1 -> SSOSQueued
        2 -> SSOSRunning
        3 -> SSOSStopped
        4 -> SSOSStopping
        5 -> SSOSSucceeded
        _ -> (error . showText) $ "Unknown index for StackSetOperationStatus: " <> toText i
    fromEnum x = case x of
        SSOSFailed -> 0
        SSOSQueued -> 1
        SSOSRunning -> 2
        SSOSStopped -> 3
        SSOSStopping -> 4
        SSOSSucceeded -> 5
        StackSetOperationStatus' name -> (error . showText) $ "Unknown StackSetOperationStatus: " <> original name

-- | Represents the bounds of /known/ $StackSetOperationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StackSetOperationStatus where
    minBound = SSOSFailed
    maxBound = SSOSSucceeded

instance Hashable     StackSetOperationStatus
instance NFData       StackSetOperationStatus
instance ToByteString StackSetOperationStatus
instance ToQuery      StackSetOperationStatus
instance ToHeader     StackSetOperationStatus

instance FromXML StackSetOperationStatus where
    parseXML = parseXMLText "StackSetOperationStatus"
