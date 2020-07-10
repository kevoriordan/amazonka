{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentRunState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.AssessmentRunState (
  AssessmentRunState (
    ..
    , Canceled
    , CollectingData
    , Completed
    , CompletedWithErrors
    , Created
    , DataCollected
    , Error'
    , EvaluatingRules
    , Failed
    , StartDataCollectionInProgress
    , StartDataCollectionPending
    , StartEvaluatingRulesPending
    , StopDataCollectionPending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AssessmentRunState = AssessmentRunState' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern Canceled :: AssessmentRunState
pattern Canceled = AssessmentRunState' "CANCELED"

pattern CollectingData :: AssessmentRunState
pattern CollectingData = AssessmentRunState' "COLLECTING_DATA"

pattern Completed :: AssessmentRunState
pattern Completed = AssessmentRunState' "COMPLETED"

pattern CompletedWithErrors :: AssessmentRunState
pattern CompletedWithErrors = AssessmentRunState' "COMPLETED_WITH_ERRORS"

pattern Created :: AssessmentRunState
pattern Created = AssessmentRunState' "CREATED"

pattern DataCollected :: AssessmentRunState
pattern DataCollected = AssessmentRunState' "DATA_COLLECTED"

pattern Error' :: AssessmentRunState
pattern Error' = AssessmentRunState' "ERROR"

pattern EvaluatingRules :: AssessmentRunState
pattern EvaluatingRules = AssessmentRunState' "EVALUATING_RULES"

pattern Failed :: AssessmentRunState
pattern Failed = AssessmentRunState' "FAILED"

pattern StartDataCollectionInProgress :: AssessmentRunState
pattern StartDataCollectionInProgress = AssessmentRunState' "START_DATA_COLLECTION_IN_PROGRESS"

pattern StartDataCollectionPending :: AssessmentRunState
pattern StartDataCollectionPending = AssessmentRunState' "START_DATA_COLLECTION_PENDING"

pattern StartEvaluatingRulesPending :: AssessmentRunState
pattern StartEvaluatingRulesPending = AssessmentRunState' "START_EVALUATING_RULES_PENDING"

pattern StopDataCollectionPending :: AssessmentRunState
pattern StopDataCollectionPending = AssessmentRunState' "STOP_DATA_COLLECTION_PENDING"

{-# COMPLETE
  Canceled,
  CollectingData,
  Completed,
  CompletedWithErrors,
  Created,
  DataCollected,
  Error',
  EvaluatingRules,
  Failed,
  StartDataCollectionInProgress,
  StartDataCollectionPending,
  StartEvaluatingRulesPending,
  StopDataCollectionPending,
  AssessmentRunState' #-}

instance FromText AssessmentRunState where
    parser = (AssessmentRunState' . mk) <$> takeText

instance ToText AssessmentRunState where
    toText (AssessmentRunState' ci) = original ci

-- | Represents an enum of /known/ $AssessmentRunState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AssessmentRunState where
    toEnum i = case i of
        0 -> Canceled
        1 -> CollectingData
        2 -> Completed
        3 -> CompletedWithErrors
        4 -> Created
        5 -> DataCollected
        6 -> Error'
        7 -> EvaluatingRules
        8 -> Failed
        9 -> StartDataCollectionInProgress
        10 -> StartDataCollectionPending
        11 -> StartEvaluatingRulesPending
        12 -> StopDataCollectionPending
        _ -> (error . showText) $ "Unknown index for AssessmentRunState: " <> toText i
    fromEnum x = case x of
        Canceled -> 0
        CollectingData -> 1
        Completed -> 2
        CompletedWithErrors -> 3
        Created -> 4
        DataCollected -> 5
        Error' -> 6
        EvaluatingRules -> 7
        Failed -> 8
        StartDataCollectionInProgress -> 9
        StartDataCollectionPending -> 10
        StartEvaluatingRulesPending -> 11
        StopDataCollectionPending -> 12
        AssessmentRunState' name -> (error . showText) $ "Unknown AssessmentRunState: " <> original name

-- | Represents the bounds of /known/ $AssessmentRunState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AssessmentRunState where
    minBound = Canceled
    maxBound = StopDataCollectionPending

instance Hashable     AssessmentRunState
instance NFData       AssessmentRunState
instance ToByteString AssessmentRunState
instance ToQuery      AssessmentRunState
instance ToHeader     AssessmentRunState

instance ToJSON AssessmentRunState where
    toJSON = toJSONText

instance FromJSON AssessmentRunState where
    parseJSON = parseJSONText "AssessmentRunState"
