{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetOperationResultStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.StackSetOperationResultStatus (
  StackSetOperationResultStatus (
    ..
    , Cancelled
    , Failed
    , Pending
    , Running
    , Succeeded
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StackSetOperationResultStatus = StackSetOperationResultStatus' (CI
                                                                       Text)
                                       deriving (Eq, Ord, Read, Show, Data,
                                                 Typeable, Generic)

pattern Cancelled :: StackSetOperationResultStatus
pattern Cancelled = StackSetOperationResultStatus' "CANCELLED"

pattern Failed :: StackSetOperationResultStatus
pattern Failed = StackSetOperationResultStatus' "FAILED"

pattern Pending :: StackSetOperationResultStatus
pattern Pending = StackSetOperationResultStatus' "PENDING"

pattern Running :: StackSetOperationResultStatus
pattern Running = StackSetOperationResultStatus' "RUNNING"

pattern Succeeded :: StackSetOperationResultStatus
pattern Succeeded = StackSetOperationResultStatus' "SUCCEEDED"

{-# COMPLETE
  Cancelled,
  Failed,
  Pending,
  Running,
  Succeeded,
  StackSetOperationResultStatus' #-}

instance FromText StackSetOperationResultStatus where
    parser = (StackSetOperationResultStatus' . mk) <$> takeText

instance ToText StackSetOperationResultStatus where
    toText (StackSetOperationResultStatus' ci) = original ci

-- | Represents an enum of /known/ $StackSetOperationResultStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StackSetOperationResultStatus where
    toEnum i = case i of
        0 -> Cancelled
        1 -> Failed
        2 -> Pending
        3 -> Running
        4 -> Succeeded
        _ -> (error . showText) $ "Unknown index for StackSetOperationResultStatus: " <> toText i
    fromEnum x = case x of
        Cancelled -> 0
        Failed -> 1
        Pending -> 2
        Running -> 3
        Succeeded -> 4
        StackSetOperationResultStatus' name -> (error . showText) $ "Unknown StackSetOperationResultStatus: " <> original name

-- | Represents the bounds of /known/ $StackSetOperationResultStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StackSetOperationResultStatus where
    minBound = Cancelled
    maxBound = Succeeded

instance Hashable     StackSetOperationResultStatus
instance NFData       StackSetOperationResultStatus
instance ToByteString StackSetOperationResultStatus
instance ToQuery      StackSetOperationResultStatus
instance ToHeader     StackSetOperationResultStatus

instance FromXML StackSetOperationResultStatus where
    parseXML = parseXMLText "StackSetOperationResultStatus"
