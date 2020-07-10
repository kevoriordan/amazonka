{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.InspectorEvent
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.InspectorEvent (
  InspectorEvent (
    ..
    , AssessmentRunCompleted
    , AssessmentRunStarted
    , AssessmentRunStateChanged
    , FindingReported
    , Other
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data InspectorEvent = InspectorEvent' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern AssessmentRunCompleted :: InspectorEvent
pattern AssessmentRunCompleted = InspectorEvent' "ASSESSMENT_RUN_COMPLETED"

pattern AssessmentRunStarted :: InspectorEvent
pattern AssessmentRunStarted = InspectorEvent' "ASSESSMENT_RUN_STARTED"

pattern AssessmentRunStateChanged :: InspectorEvent
pattern AssessmentRunStateChanged = InspectorEvent' "ASSESSMENT_RUN_STATE_CHANGED"

pattern FindingReported :: InspectorEvent
pattern FindingReported = InspectorEvent' "FINDING_REPORTED"

pattern Other :: InspectorEvent
pattern Other = InspectorEvent' "OTHER"

{-# COMPLETE
  AssessmentRunCompleted,
  AssessmentRunStarted,
  AssessmentRunStateChanged,
  FindingReported,
  Other,
  InspectorEvent' #-}

instance FromText InspectorEvent where
    parser = (InspectorEvent' . mk) <$> takeText

instance ToText InspectorEvent where
    toText (InspectorEvent' ci) = original ci

-- | Represents an enum of /known/ $InspectorEvent.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InspectorEvent where
    toEnum i = case i of
        0 -> AssessmentRunCompleted
        1 -> AssessmentRunStarted
        2 -> AssessmentRunStateChanged
        3 -> FindingReported
        4 -> Other
        _ -> (error . showText) $ "Unknown index for InspectorEvent: " <> toText i
    fromEnum x = case x of
        AssessmentRunCompleted -> 0
        AssessmentRunStarted -> 1
        AssessmentRunStateChanged -> 2
        FindingReported -> 3
        Other -> 4
        InspectorEvent' name -> (error . showText) $ "Unknown InspectorEvent: " <> original name

-- | Represents the bounds of /known/ $InspectorEvent.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InspectorEvent where
    minBound = AssessmentRunCompleted
    maxBound = Other

instance Hashable     InspectorEvent
instance NFData       InspectorEvent
instance ToByteString InspectorEvent
instance ToQuery      InspectorEvent
instance ToHeader     InspectorEvent

instance ToJSON InspectorEvent where
    toJSON = toJSONText

instance FromJSON InspectorEvent where
    parseJSON = parseJSONText "InspectorEvent"
