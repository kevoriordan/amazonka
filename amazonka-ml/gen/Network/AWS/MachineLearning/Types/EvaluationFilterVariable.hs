{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.EvaluationFilterVariable
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MachineLearning.Types.EvaluationFilterVariable (
  EvaluationFilterVariable (
    ..
    , EvalCreatedAt
    , EvalDataSourceId
    , EvalDataURI
    , EvalIAMUser
    , EvalLastUpdatedAt
    , EvalMLModelId
    , EvalName
    , EvalStatus
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | A list of the variables to use in searching or filtering @Evaluation@ .
--
--
--     * @CreatedAt@ - Sets the search criteria to @Evaluation@ creation date.    * @Status@ - Sets the search criteria to @Evaluation@ status.    * @Name@ - Sets the search criteria to the contents of @Evaluation@ ____ @Name@ .    * @IAMUser@ - Sets the search criteria to the user account that invoked an evaluation.    * @MLModelId@ - Sets the search criteria to the @Predictor@ that was evaluated.    * @DataSourceId@ - Sets the search criteria to the @DataSource@ used in evaluation.    * @DataUri@ - Sets the search criteria to the data file(s) used in evaluation. The URL can identify either a file or an Amazon Simple Storage Service (Amazon S3) bucket or directory.
--
data EvaluationFilterVariable = EvaluationFilterVariable' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern EvalCreatedAt :: EvaluationFilterVariable
pattern EvalCreatedAt = EvaluationFilterVariable' "CreatedAt"

pattern EvalDataSourceId :: EvaluationFilterVariable
pattern EvalDataSourceId = EvaluationFilterVariable' "DataSourceId"

pattern EvalDataURI :: EvaluationFilterVariable
pattern EvalDataURI = EvaluationFilterVariable' "DataURI"

pattern EvalIAMUser :: EvaluationFilterVariable
pattern EvalIAMUser = EvaluationFilterVariable' "IAMUser"

pattern EvalLastUpdatedAt :: EvaluationFilterVariable
pattern EvalLastUpdatedAt = EvaluationFilterVariable' "LastUpdatedAt"

pattern EvalMLModelId :: EvaluationFilterVariable
pattern EvalMLModelId = EvaluationFilterVariable' "MLModelId"

pattern EvalName :: EvaluationFilterVariable
pattern EvalName = EvaluationFilterVariable' "Name"

pattern EvalStatus :: EvaluationFilterVariable
pattern EvalStatus = EvaluationFilterVariable' "Status"

{-# COMPLETE
  EvalCreatedAt,
  EvalDataSourceId,
  EvalDataURI,
  EvalIAMUser,
  EvalLastUpdatedAt,
  EvalMLModelId,
  EvalName,
  EvalStatus,
  EvaluationFilterVariable' #-}

instance FromText EvaluationFilterVariable where
    parser = (EvaluationFilterVariable' . mk) <$> takeText

instance ToText EvaluationFilterVariable where
    toText (EvaluationFilterVariable' ci) = original ci

-- | Represents an enum of /known/ $EvaluationFilterVariable.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EvaluationFilterVariable where
    toEnum i = case i of
        0 -> EvalCreatedAt
        1 -> EvalDataSourceId
        2 -> EvalDataURI
        3 -> EvalIAMUser
        4 -> EvalLastUpdatedAt
        5 -> EvalMLModelId
        6 -> EvalName
        7 -> EvalStatus
        _ -> (error . showText) $ "Unknown index for EvaluationFilterVariable: " <> toText i
    fromEnum x = case x of
        EvalCreatedAt -> 0
        EvalDataSourceId -> 1
        EvalDataURI -> 2
        EvalIAMUser -> 3
        EvalLastUpdatedAt -> 4
        EvalMLModelId -> 5
        EvalName -> 6
        EvalStatus -> 7
        EvaluationFilterVariable' name -> (error . showText) $ "Unknown EvaluationFilterVariable: " <> original name

-- | Represents the bounds of /known/ $EvaluationFilterVariable.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EvaluationFilterVariable where
    minBound = EvalCreatedAt
    maxBound = EvalStatus

instance Hashable     EvaluationFilterVariable
instance NFData       EvaluationFilterVariable
instance ToByteString EvaluationFilterVariable
instance ToQuery      EvaluationFilterVariable
instance ToHeader     EvaluationFilterVariable

instance ToJSON EvaluationFilterVariable where
    toJSON = toJSONText
