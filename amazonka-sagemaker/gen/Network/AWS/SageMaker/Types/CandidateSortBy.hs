{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CandidateSortBy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.CandidateSortBy (
  CandidateSortBy (
    ..
    , CSBCreationTime
    , CSBFinalObjectiveMetricValue
    , CSBStatus
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CandidateSortBy = CandidateSortBy' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern CSBCreationTime :: CandidateSortBy
pattern CSBCreationTime = CandidateSortBy' "CreationTime"

pattern CSBFinalObjectiveMetricValue :: CandidateSortBy
pattern CSBFinalObjectiveMetricValue = CandidateSortBy' "FinalObjectiveMetricValue"

pattern CSBStatus :: CandidateSortBy
pattern CSBStatus = CandidateSortBy' "Status"

{-# COMPLETE
  CSBCreationTime,
  CSBFinalObjectiveMetricValue,
  CSBStatus,
  CandidateSortBy' #-}

instance FromText CandidateSortBy where
    parser = (CandidateSortBy' . mk) <$> takeText

instance ToText CandidateSortBy where
    toText (CandidateSortBy' ci) = original ci

-- | Represents an enum of /known/ $CandidateSortBy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CandidateSortBy where
    toEnum i = case i of
        0 -> CSBCreationTime
        1 -> CSBFinalObjectiveMetricValue
        2 -> CSBStatus
        _ -> (error . showText) $ "Unknown index for CandidateSortBy: " <> toText i
    fromEnum x = case x of
        CSBCreationTime -> 0
        CSBFinalObjectiveMetricValue -> 1
        CSBStatus -> 2
        CandidateSortBy' name -> (error . showText) $ "Unknown CandidateSortBy: " <> original name

-- | Represents the bounds of /known/ $CandidateSortBy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CandidateSortBy where
    minBound = CSBCreationTime
    maxBound = CSBStatus

instance Hashable     CandidateSortBy
instance NFData       CandidateSortBy
instance ToByteString CandidateSortBy
instance ToQuery      CandidateSortBy
instance ToHeader     CandidateSortBy

instance ToJSON CandidateSortBy where
    toJSON = toJSONText
