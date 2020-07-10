{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.PipelineExecutionStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.PipelineExecutionStatus (
  PipelineExecutionStatus (
    ..
    , Failed
    , InProgress
    , Stopped
    , Stopping
    , Succeeded
    , Superseded
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PipelineExecutionStatus = PipelineExecutionStatus' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern Failed :: PipelineExecutionStatus
pattern Failed = PipelineExecutionStatus' "Failed"

pattern InProgress :: PipelineExecutionStatus
pattern InProgress = PipelineExecutionStatus' "InProgress"

pattern Stopped :: PipelineExecutionStatus
pattern Stopped = PipelineExecutionStatus' "Stopped"

pattern Stopping :: PipelineExecutionStatus
pattern Stopping = PipelineExecutionStatus' "Stopping"

pattern Succeeded :: PipelineExecutionStatus
pattern Succeeded = PipelineExecutionStatus' "Succeeded"

pattern Superseded :: PipelineExecutionStatus
pattern Superseded = PipelineExecutionStatus' "Superseded"

{-# COMPLETE
  Failed,
  InProgress,
  Stopped,
  Stopping,
  Succeeded,
  Superseded,
  PipelineExecutionStatus' #-}

instance FromText PipelineExecutionStatus where
    parser = (PipelineExecutionStatus' . mk) <$> takeText

instance ToText PipelineExecutionStatus where
    toText (PipelineExecutionStatus' ci) = original ci

-- | Represents an enum of /known/ $PipelineExecutionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PipelineExecutionStatus where
    toEnum i = case i of
        0 -> Failed
        1 -> InProgress
        2 -> Stopped
        3 -> Stopping
        4 -> Succeeded
        5 -> Superseded
        _ -> (error . showText) $ "Unknown index for PipelineExecutionStatus: " <> toText i
    fromEnum x = case x of
        Failed -> 0
        InProgress -> 1
        Stopped -> 2
        Stopping -> 3
        Succeeded -> 4
        Superseded -> 5
        PipelineExecutionStatus' name -> (error . showText) $ "Unknown PipelineExecutionStatus: " <> original name

-- | Represents the bounds of /known/ $PipelineExecutionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PipelineExecutionStatus where
    minBound = Failed
    maxBound = Superseded

instance Hashable     PipelineExecutionStatus
instance NFData       PipelineExecutionStatus
instance ToByteString PipelineExecutionStatus
instance ToQuery      PipelineExecutionStatus
instance ToHeader     PipelineExecutionStatus

instance FromJSON PipelineExecutionStatus where
    parseJSON = parseJSONText "PipelineExecutionStatus"
