{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.Product (
    module Network.AWS.Comprehend.Types.BatchDetectDominantLanguageItemResult,
    module Network.AWS.Comprehend.Types.BatchDetectEntitiesItemResult,
    module Network.AWS.Comprehend.Types.BatchDetectKeyPhrasesItemResult,
    module Network.AWS.Comprehend.Types.BatchDetectSentimentItemResult,
    module Network.AWS.Comprehend.Types.BatchDetectSyntaxItemResult,
    module Network.AWS.Comprehend.Types.BatchItemError,
    module Network.AWS.Comprehend.Types.ClassifierEvaluationMetrics,
    module Network.AWS.Comprehend.Types.ClassifierMetadata,
    module Network.AWS.Comprehend.Types.DocumentClass,
    module Network.AWS.Comprehend.Types.DocumentClassificationJobFilter,
    module Network.AWS.Comprehend.Types.DocumentClassificationJobProperties,
    module Network.AWS.Comprehend.Types.DocumentClassifierFilter,
    module Network.AWS.Comprehend.Types.DocumentClassifierInputDataConfig,
    module Network.AWS.Comprehend.Types.DocumentClassifierOutputDataConfig,
    module Network.AWS.Comprehend.Types.DocumentClassifierProperties,
    module Network.AWS.Comprehend.Types.DocumentLabel,
    module Network.AWS.Comprehend.Types.DominantLanguage,
    module Network.AWS.Comprehend.Types.DominantLanguageDetectionJobFilter,
    module Network.AWS.Comprehend.Types.DominantLanguageDetectionJobProperties,
    module Network.AWS.Comprehend.Types.EndpointFilter,
    module Network.AWS.Comprehend.Types.EndpointProperties,
    module Network.AWS.Comprehend.Types.EntitiesDetectionJobFilter,
    module Network.AWS.Comprehend.Types.EntitiesDetectionJobProperties,
    module Network.AWS.Comprehend.Types.Entity,
    module Network.AWS.Comprehend.Types.EntityRecognizerAnnotations,
    module Network.AWS.Comprehend.Types.EntityRecognizerDocuments,
    module Network.AWS.Comprehend.Types.EntityRecognizerEntityList,
    module Network.AWS.Comprehend.Types.EntityRecognizerEvaluationMetrics,
    module Network.AWS.Comprehend.Types.EntityRecognizerFilter,
    module Network.AWS.Comprehend.Types.EntityRecognizerInputDataConfig,
    module Network.AWS.Comprehend.Types.EntityRecognizerMetadata,
    module Network.AWS.Comprehend.Types.EntityRecognizerMetadataEntityTypesListItem,
    module Network.AWS.Comprehend.Types.EntityRecognizerProperties,
    module Network.AWS.Comprehend.Types.EntityTypesEvaluationMetrics,
    module Network.AWS.Comprehend.Types.EntityTypesListItem,
    module Network.AWS.Comprehend.Types.InputDataConfig,
    module Network.AWS.Comprehend.Types.KeyPhrase,
    module Network.AWS.Comprehend.Types.KeyPhrasesDetectionJobFilter,
    module Network.AWS.Comprehend.Types.KeyPhrasesDetectionJobProperties,
    module Network.AWS.Comprehend.Types.OutputDataConfig,
    module Network.AWS.Comprehend.Types.PartOfSpeechTag,
    module Network.AWS.Comprehend.Types.SentimentDetectionJobFilter,
    module Network.AWS.Comprehend.Types.SentimentDetectionJobProperties,
    module Network.AWS.Comprehend.Types.SentimentScore,
    module Network.AWS.Comprehend.Types.SyntaxToken,
    module Network.AWS.Comprehend.Types.Tag,
    module Network.AWS.Comprehend.Types.TopicsDetectionJobFilter,
    module Network.AWS.Comprehend.Types.TopicsDetectionJobProperties,
    module Network.AWS.Comprehend.Types.VPCConfig
  ) where

import Network.AWS.Comprehend.Types.BatchDetectDominantLanguageItemResult
import Network.AWS.Comprehend.Types.BatchDetectEntitiesItemResult
import Network.AWS.Comprehend.Types.BatchDetectKeyPhrasesItemResult
import Network.AWS.Comprehend.Types.BatchDetectSentimentItemResult
import Network.AWS.Comprehend.Types.BatchDetectSyntaxItemResult
import Network.AWS.Comprehend.Types.BatchItemError
import Network.AWS.Comprehend.Types.ClassifierEvaluationMetrics
import Network.AWS.Comprehend.Types.ClassifierMetadata
import Network.AWS.Comprehend.Types.DocumentClass
import Network.AWS.Comprehend.Types.DocumentClassificationJobFilter
import Network.AWS.Comprehend.Types.DocumentClassificationJobProperties
import Network.AWS.Comprehend.Types.DocumentClassifierFilter
import Network.AWS.Comprehend.Types.DocumentClassifierInputDataConfig
import Network.AWS.Comprehend.Types.DocumentClassifierOutputDataConfig
import Network.AWS.Comprehend.Types.DocumentClassifierProperties
import Network.AWS.Comprehend.Types.DocumentLabel
import Network.AWS.Comprehend.Types.DominantLanguage
import Network.AWS.Comprehend.Types.DominantLanguageDetectionJobFilter
import Network.AWS.Comprehend.Types.DominantLanguageDetectionJobProperties
import Network.AWS.Comprehend.Types.EndpointFilter
import Network.AWS.Comprehend.Types.EndpointProperties
import Network.AWS.Comprehend.Types.EntitiesDetectionJobFilter
import Network.AWS.Comprehend.Types.EntitiesDetectionJobProperties
import Network.AWS.Comprehend.Types.Entity
import Network.AWS.Comprehend.Types.EntityRecognizerAnnotations
import Network.AWS.Comprehend.Types.EntityRecognizerDocuments
import Network.AWS.Comprehend.Types.EntityRecognizerEntityList
import Network.AWS.Comprehend.Types.EntityRecognizerEvaluationMetrics
import Network.AWS.Comprehend.Types.EntityRecognizerFilter
import Network.AWS.Comprehend.Types.EntityRecognizerInputDataConfig
import Network.AWS.Comprehend.Types.EntityRecognizerMetadata
import Network.AWS.Comprehend.Types.EntityRecognizerMetadataEntityTypesListItem
import Network.AWS.Comprehend.Types.EntityRecognizerProperties
import Network.AWS.Comprehend.Types.EntityTypesEvaluationMetrics
import Network.AWS.Comprehend.Types.EntityTypesListItem
import Network.AWS.Comprehend.Types.InputDataConfig
import Network.AWS.Comprehend.Types.KeyPhrase
import Network.AWS.Comprehend.Types.KeyPhrasesDetectionJobFilter
import Network.AWS.Comprehend.Types.KeyPhrasesDetectionJobProperties
import Network.AWS.Comprehend.Types.OutputDataConfig
import Network.AWS.Comprehend.Types.PartOfSpeechTag
import Network.AWS.Comprehend.Types.SentimentDetectionJobFilter
import Network.AWS.Comprehend.Types.SentimentDetectionJobProperties
import Network.AWS.Comprehend.Types.SentimentScore
import Network.AWS.Comprehend.Types.SyntaxToken
import Network.AWS.Comprehend.Types.Tag
import Network.AWS.Comprehend.Types.TopicsDetectionJobFilter
import Network.AWS.Comprehend.Types.TopicsDetectionJobProperties
import Network.AWS.Comprehend.Types.VPCConfig
import Network.AWS.Lens
import Network.AWS.Prelude
