{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.TriggerType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.TriggerType (
  TriggerType (
    ..
    , CloudWatchEvent
    , CreatePipeline
    , PollForSourceChanges
    , PutActionRevision
    , StartPipelineExecution
    , Webhook
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TriggerType = TriggerType' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern CloudWatchEvent :: TriggerType
pattern CloudWatchEvent = TriggerType' "CloudWatchEvent"

pattern CreatePipeline :: TriggerType
pattern CreatePipeline = TriggerType' "CreatePipeline"

pattern PollForSourceChanges :: TriggerType
pattern PollForSourceChanges = TriggerType' "PollForSourceChanges"

pattern PutActionRevision :: TriggerType
pattern PutActionRevision = TriggerType' "PutActionRevision"

pattern StartPipelineExecution :: TriggerType
pattern StartPipelineExecution = TriggerType' "StartPipelineExecution"

pattern Webhook :: TriggerType
pattern Webhook = TriggerType' "Webhook"

{-# COMPLETE
  CloudWatchEvent,
  CreatePipeline,
  PollForSourceChanges,
  PutActionRevision,
  StartPipelineExecution,
  Webhook,
  TriggerType' #-}

instance FromText TriggerType where
    parser = (TriggerType' . mk) <$> takeText

instance ToText TriggerType where
    toText (TriggerType' ci) = original ci

-- | Represents an enum of /known/ $TriggerType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TriggerType where
    toEnum i = case i of
        0 -> CloudWatchEvent
        1 -> CreatePipeline
        2 -> PollForSourceChanges
        3 -> PutActionRevision
        4 -> StartPipelineExecution
        5 -> Webhook
        _ -> (error . showText) $ "Unknown index for TriggerType: " <> toText i
    fromEnum x = case x of
        CloudWatchEvent -> 0
        CreatePipeline -> 1
        PollForSourceChanges -> 2
        PutActionRevision -> 3
        StartPipelineExecution -> 4
        Webhook -> 5
        TriggerType' name -> (error . showText) $ "Unknown TriggerType: " <> original name

-- | Represents the bounds of /known/ $TriggerType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TriggerType where
    minBound = CloudWatchEvent
    maxBound = Webhook

instance Hashable     TriggerType
instance NFData       TriggerType
instance ToByteString TriggerType
instance ToQuery      TriggerType
instance ToHeader     TriggerType

instance FromJSON TriggerType where
    parseJSON = parseJSONText "TriggerType"
