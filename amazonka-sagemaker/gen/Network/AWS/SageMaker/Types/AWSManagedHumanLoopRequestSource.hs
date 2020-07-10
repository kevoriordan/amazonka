{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AWSManagedHumanLoopRequestSource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AWSManagedHumanLoopRequestSource (
  AWSManagedHumanLoopRequestSource (
    ..
    , AWSRekognitionDetectModerationLabelsImageV3
    , AWSTextractAnalyzeDocumentFormsV1
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AWSManagedHumanLoopRequestSource = AWSManagedHumanLoopRequestSource' (CI
                                                                             Text)
                                          deriving (Eq, Ord, Read, Show, Data,
                                                    Typeable, Generic)

pattern AWSRekognitionDetectModerationLabelsImageV3 :: AWSManagedHumanLoopRequestSource
pattern AWSRekognitionDetectModerationLabelsImageV3 = AWSManagedHumanLoopRequestSource' "AWS/Rekognition/DetectModerationLabels/Image/V3"

pattern AWSTextractAnalyzeDocumentFormsV1 :: AWSManagedHumanLoopRequestSource
pattern AWSTextractAnalyzeDocumentFormsV1 = AWSManagedHumanLoopRequestSource' "AWS/Textract/AnalyzeDocument/Forms/V1"

{-# COMPLETE
  AWSRekognitionDetectModerationLabelsImageV3,
  AWSTextractAnalyzeDocumentFormsV1,
  AWSManagedHumanLoopRequestSource' #-}

instance FromText AWSManagedHumanLoopRequestSource where
    parser = (AWSManagedHumanLoopRequestSource' . mk) <$> takeText

instance ToText AWSManagedHumanLoopRequestSource where
    toText (AWSManagedHumanLoopRequestSource' ci) = original ci

-- | Represents an enum of /known/ $AWSManagedHumanLoopRequestSource.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AWSManagedHumanLoopRequestSource where
    toEnum i = case i of
        0 -> AWSRekognitionDetectModerationLabelsImageV3
        1 -> AWSTextractAnalyzeDocumentFormsV1
        _ -> (error . showText) $ "Unknown index for AWSManagedHumanLoopRequestSource: " <> toText i
    fromEnum x = case x of
        AWSRekognitionDetectModerationLabelsImageV3 -> 0
        AWSTextractAnalyzeDocumentFormsV1 -> 1
        AWSManagedHumanLoopRequestSource' name -> (error . showText) $ "Unknown AWSManagedHumanLoopRequestSource: " <> original name

-- | Represents the bounds of /known/ $AWSManagedHumanLoopRequestSource.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AWSManagedHumanLoopRequestSource where
    minBound = AWSRekognitionDetectModerationLabelsImageV3
    maxBound = AWSTextractAnalyzeDocumentFormsV1

instance Hashable     AWSManagedHumanLoopRequestSource
instance NFData       AWSManagedHumanLoopRequestSource
instance ToByteString AWSManagedHumanLoopRequestSource
instance ToQuery      AWSManagedHumanLoopRequestSource
instance ToHeader     AWSManagedHumanLoopRequestSource

instance ToJSON AWSManagedHumanLoopRequestSource where
    toJSON = toJSONText

instance FromJSON AWSManagedHumanLoopRequestSource where
    parseJSON = parseJSONText "AWSManagedHumanLoopRequestSource"
