{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.RuleEvaluationStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.RuleEvaluationStatus (
  RuleEvaluationStatus (
    ..
    , Error'
    , InProgress
    , IssuesFound
    , NoIssuesFound
    , Stopped
    , Stopping
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RuleEvaluationStatus = RuleEvaluationStatus' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern Error' :: RuleEvaluationStatus
pattern Error' = RuleEvaluationStatus' "Error"

pattern InProgress :: RuleEvaluationStatus
pattern InProgress = RuleEvaluationStatus' "InProgress"

pattern IssuesFound :: RuleEvaluationStatus
pattern IssuesFound = RuleEvaluationStatus' "IssuesFound"

pattern NoIssuesFound :: RuleEvaluationStatus
pattern NoIssuesFound = RuleEvaluationStatus' "NoIssuesFound"

pattern Stopped :: RuleEvaluationStatus
pattern Stopped = RuleEvaluationStatus' "Stopped"

pattern Stopping :: RuleEvaluationStatus
pattern Stopping = RuleEvaluationStatus' "Stopping"

{-# COMPLETE
  Error',
  InProgress,
  IssuesFound,
  NoIssuesFound,
  Stopped,
  Stopping,
  RuleEvaluationStatus' #-}

instance FromText RuleEvaluationStatus where
    parser = (RuleEvaluationStatus' . mk) <$> takeText

instance ToText RuleEvaluationStatus where
    toText (RuleEvaluationStatus' ci) = original ci

-- | Represents an enum of /known/ $RuleEvaluationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RuleEvaluationStatus where
    toEnum i = case i of
        0 -> Error'
        1 -> InProgress
        2 -> IssuesFound
        3 -> NoIssuesFound
        4 -> Stopped
        5 -> Stopping
        _ -> (error . showText) $ "Unknown index for RuleEvaluationStatus: " <> toText i
    fromEnum x = case x of
        Error' -> 0
        InProgress -> 1
        IssuesFound -> 2
        NoIssuesFound -> 3
        Stopped -> 4
        Stopping -> 5
        RuleEvaluationStatus' name -> (error . showText) $ "Unknown RuleEvaluationStatus: " <> original name

-- | Represents the bounds of /known/ $RuleEvaluationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RuleEvaluationStatus where
    minBound = Error'
    maxBound = Stopping

instance Hashable     RuleEvaluationStatus
instance NFData       RuleEvaluationStatus
instance ToByteString RuleEvaluationStatus
instance ToQuery      RuleEvaluationStatus
instance ToHeader     RuleEvaluationStatus

instance FromJSON RuleEvaluationStatus where
    parseJSON = parseJSONText "RuleEvaluationStatus"
