{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.MLModelFilterVariable
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MachineLearning.Types.MLModelFilterVariable (
  MLModelFilterVariable (
    ..
    , MLMFVAlgorithm
    , MLMFVCreatedAt
    , MLMFVIAMUser
    , MLMFVLastUpdatedAt
    , MLMFVMLModelType
    , MLMFVName
    , MLMFVRealtimeEndpointStatus
    , MLMFVStatus
    , MLMFVTrainingDataSourceId
    , MLMFVTrainingDataURI
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MLModelFilterVariable = MLModelFilterVariable' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern MLMFVAlgorithm :: MLModelFilterVariable
pattern MLMFVAlgorithm = MLModelFilterVariable' "Algorithm"

pattern MLMFVCreatedAt :: MLModelFilterVariable
pattern MLMFVCreatedAt = MLModelFilterVariable' "CreatedAt"

pattern MLMFVIAMUser :: MLModelFilterVariable
pattern MLMFVIAMUser = MLModelFilterVariable' "IAMUser"

pattern MLMFVLastUpdatedAt :: MLModelFilterVariable
pattern MLMFVLastUpdatedAt = MLModelFilterVariable' "LastUpdatedAt"

pattern MLMFVMLModelType :: MLModelFilterVariable
pattern MLMFVMLModelType = MLModelFilterVariable' "MLModelType"

pattern MLMFVName :: MLModelFilterVariable
pattern MLMFVName = MLModelFilterVariable' "Name"

pattern MLMFVRealtimeEndpointStatus :: MLModelFilterVariable
pattern MLMFVRealtimeEndpointStatus = MLModelFilterVariable' "RealtimeEndpointStatus"

pattern MLMFVStatus :: MLModelFilterVariable
pattern MLMFVStatus = MLModelFilterVariable' "Status"

pattern MLMFVTrainingDataSourceId :: MLModelFilterVariable
pattern MLMFVTrainingDataSourceId = MLModelFilterVariable' "TrainingDataSourceId"

pattern MLMFVTrainingDataURI :: MLModelFilterVariable
pattern MLMFVTrainingDataURI = MLModelFilterVariable' "TrainingDataURI"

{-# COMPLETE
  MLMFVAlgorithm,
  MLMFVCreatedAt,
  MLMFVIAMUser,
  MLMFVLastUpdatedAt,
  MLMFVMLModelType,
  MLMFVName,
  MLMFVRealtimeEndpointStatus,
  MLMFVStatus,
  MLMFVTrainingDataSourceId,
  MLMFVTrainingDataURI,
  MLModelFilterVariable' #-}

instance FromText MLModelFilterVariable where
    parser = (MLModelFilterVariable' . mk) <$> takeText

instance ToText MLModelFilterVariable where
    toText (MLModelFilterVariable' ci) = original ci

-- | Represents an enum of /known/ $MLModelFilterVariable.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MLModelFilterVariable where
    toEnum i = case i of
        0 -> MLMFVAlgorithm
        1 -> MLMFVCreatedAt
        2 -> MLMFVIAMUser
        3 -> MLMFVLastUpdatedAt
        4 -> MLMFVMLModelType
        5 -> MLMFVName
        6 -> MLMFVRealtimeEndpointStatus
        7 -> MLMFVStatus
        8 -> MLMFVTrainingDataSourceId
        9 -> MLMFVTrainingDataURI
        _ -> (error . showText) $ "Unknown index for MLModelFilterVariable: " <> toText i
    fromEnum x = case x of
        MLMFVAlgorithm -> 0
        MLMFVCreatedAt -> 1
        MLMFVIAMUser -> 2
        MLMFVLastUpdatedAt -> 3
        MLMFVMLModelType -> 4
        MLMFVName -> 5
        MLMFVRealtimeEndpointStatus -> 6
        MLMFVStatus -> 7
        MLMFVTrainingDataSourceId -> 8
        MLMFVTrainingDataURI -> 9
        MLModelFilterVariable' name -> (error . showText) $ "Unknown MLModelFilterVariable: " <> original name

-- | Represents the bounds of /known/ $MLModelFilterVariable.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MLModelFilterVariable where
    minBound = MLMFVAlgorithm
    maxBound = MLMFVTrainingDataURI

instance Hashable     MLModelFilterVariable
instance NFData       MLModelFilterVariable
instance ToByteString MLModelFilterVariable
instance ToQuery      MLModelFilterVariable
instance ToHeader     MLModelFilterVariable

instance ToJSON MLModelFilterVariable where
    toJSON = toJSONText
