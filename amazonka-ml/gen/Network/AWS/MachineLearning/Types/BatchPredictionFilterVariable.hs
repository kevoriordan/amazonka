{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.BatchPredictionFilterVariable
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MachineLearning.Types.BatchPredictionFilterVariable (
  BatchPredictionFilterVariable (
    ..
    , BatchCreatedAt
    , BatchDataSourceId
    , BatchDataURI
    , BatchIAMUser
    , BatchLastUpdatedAt
    , BatchMLModelId
    , BatchName
    , BatchStatus
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | A list of the variables to use in searching or filtering @BatchPrediction@ .
--
--
--     * @CreatedAt@ - Sets the search criteria to @BatchPrediction@ creation date.    * @Status@ - Sets the search criteria to @BatchPrediction@ status.    * @Name@ - Sets the search criteria to the contents of @BatchPrediction@ ____ @Name@ .    * @IAMUser@ - Sets the search criteria to the user account that invoked the @BatchPrediction@ creation.    * @MLModelId@ - Sets the search criteria to the @MLModel@ used in the @BatchPrediction@ .    * @DataSourceId@ - Sets the search criteria to the @DataSource@ used in the @BatchPrediction@ .    * @DataURI@ - Sets the search criteria to the data file(s) used in the @BatchPrediction@ . The URL can identify either a file or an Amazon Simple Storage Service (Amazon S3) bucket or directory.
--
data BatchPredictionFilterVariable = BatchPredictionFilterVariable' (CI
                                                                       Text)
                                       deriving (Eq, Ord, Read, Show, Data,
                                                 Typeable, Generic)

pattern BatchCreatedAt :: BatchPredictionFilterVariable
pattern BatchCreatedAt = BatchPredictionFilterVariable' "CreatedAt"

pattern BatchDataSourceId :: BatchPredictionFilterVariable
pattern BatchDataSourceId = BatchPredictionFilterVariable' "DataSourceId"

pattern BatchDataURI :: BatchPredictionFilterVariable
pattern BatchDataURI = BatchPredictionFilterVariable' "DataURI"

pattern BatchIAMUser :: BatchPredictionFilterVariable
pattern BatchIAMUser = BatchPredictionFilterVariable' "IAMUser"

pattern BatchLastUpdatedAt :: BatchPredictionFilterVariable
pattern BatchLastUpdatedAt = BatchPredictionFilterVariable' "LastUpdatedAt"

pattern BatchMLModelId :: BatchPredictionFilterVariable
pattern BatchMLModelId = BatchPredictionFilterVariable' "MLModelId"

pattern BatchName :: BatchPredictionFilterVariable
pattern BatchName = BatchPredictionFilterVariable' "Name"

pattern BatchStatus :: BatchPredictionFilterVariable
pattern BatchStatus = BatchPredictionFilterVariable' "Status"

{-# COMPLETE
  BatchCreatedAt,
  BatchDataSourceId,
  BatchDataURI,
  BatchIAMUser,
  BatchLastUpdatedAt,
  BatchMLModelId,
  BatchName,
  BatchStatus,
  BatchPredictionFilterVariable' #-}

instance FromText BatchPredictionFilterVariable where
    parser = (BatchPredictionFilterVariable' . mk) <$> takeText

instance ToText BatchPredictionFilterVariable where
    toText (BatchPredictionFilterVariable' ci) = original ci

-- | Represents an enum of /known/ $BatchPredictionFilterVariable.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BatchPredictionFilterVariable where
    toEnum i = case i of
        0 -> BatchCreatedAt
        1 -> BatchDataSourceId
        2 -> BatchDataURI
        3 -> BatchIAMUser
        4 -> BatchLastUpdatedAt
        5 -> BatchMLModelId
        6 -> BatchName
        7 -> BatchStatus
        _ -> (error . showText) $ "Unknown index for BatchPredictionFilterVariable: " <> toText i
    fromEnum x = case x of
        BatchCreatedAt -> 0
        BatchDataSourceId -> 1
        BatchDataURI -> 2
        BatchIAMUser -> 3
        BatchLastUpdatedAt -> 4
        BatchMLModelId -> 5
        BatchName -> 6
        BatchStatus -> 7
        BatchPredictionFilterVariable' name -> (error . showText) $ "Unknown BatchPredictionFilterVariable: " <> original name

-- | Represents the bounds of /known/ $BatchPredictionFilterVariable.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BatchPredictionFilterVariable where
    minBound = BatchCreatedAt
    maxBound = BatchStatus

instance Hashable     BatchPredictionFilterVariable
instance NFData       BatchPredictionFilterVariable
instance ToByteString BatchPredictionFilterVariable
instance ToQuery      BatchPredictionFilterVariable
instance ToHeader     BatchPredictionFilterVariable

instance ToJSON BatchPredictionFilterVariable where
    toJSON = toJSONText
