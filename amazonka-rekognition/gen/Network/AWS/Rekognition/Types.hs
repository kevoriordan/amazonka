{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types
    (
    -- * Service Configuration
      rekognition

    -- * Errors
    , _IdempotentParameterMismatchException
    , _InvalidImageFormatException
    , _InvalidParameterException
    , _AccessDeniedException
    , _ImageTooLargeException
    , _InvalidPaginationTokenException
    , _HumanLoopQuotaExceededException
    , _ProvisionedThroughputExceededException
    , _ResourceNotFoundException
    , _InvalidS3ObjectException
    , _ResourceNotReadyException
    , _ResourceAlreadyExistsException
    , _InternalServerError
    , _ThrottlingException
    , _VideoTooLargeException
    , _LimitExceededException
    , _ResourceInUseException

    -- * Attribute
    , Attribute (..)

    -- * CelebrityRecognitionSortBy
    , CelebrityRecognitionSortBy (..)

    -- * ContentClassifier
    , ContentClassifier (..)

    -- * ContentModerationSortBy
    , ContentModerationSortBy (..)

    -- * EmotionName
    , EmotionName (..)

    -- * FaceAttributes
    , FaceAttributes (..)

    -- * FaceSearchSortBy
    , FaceSearchSortBy (..)

    -- * GenderType
    , GenderType (..)

    -- * LabelDetectionSortBy
    , LabelDetectionSortBy (..)

    -- * LandmarkType
    , LandmarkType (..)

    -- * OrientationCorrection
    , OrientationCorrection (..)

    -- * PersonTrackingSortBy
    , PersonTrackingSortBy (..)

    -- * ProjectStatus
    , ProjectStatus (..)

    -- * ProjectVersionStatus
    , ProjectVersionStatus (..)

    -- * QualityFilter
    , QualityFilter (..)

    -- * Reason
    , Reason (..)

    -- * StreamProcessorStatus
    , StreamProcessorStatus (..)

    -- * TextTypes
    , TextTypes (..)

    -- * VideoJobStatus
    , VideoJobStatus (..)

    -- * AgeRange
    , AgeRange
    , ageRange
    , arLow
    , arHigh

    -- * Asset
    , Asset
    , asset
    , aGroundTruthManifest

    -- * Beard
    , Beard
    , beard
    , bValue
    , bConfidence

    -- * BoundingBox
    , BoundingBox
    , boundingBox
    , bbHeight
    , bbLeft
    , bbWidth
    , bbTop

    -- * Celebrity
    , Celebrity
    , celebrity
    , cMatchConfidence
    , cURLs
    , cName
    , cId
    , cFace

    -- * CelebrityDetail
    , CelebrityDetail
    , celebrityDetail
    , cdBoundingBox
    , cdURLs
    , cdConfidence
    , cdName
    , cdId
    , cdFace

    -- * CelebrityRecognition
    , CelebrityRecognition
    , celebrityRecognition
    , crCelebrity
    , crTimestamp

    -- * CompareFacesMatch
    , CompareFacesMatch
    , compareFacesMatch
    , cfmSimilarity
    , cfmFace

    -- * ComparedFace
    , ComparedFace
    , comparedFace
    , cfBoundingBox
    , cfPose
    , cfConfidence
    , cfQuality
    , cfLandmarks

    -- * ComparedSourceImageFace
    , ComparedSourceImageFace
    , comparedSourceImageFace
    , csifBoundingBox
    , csifConfidence

    -- * ContentModerationDetection
    , ContentModerationDetection
    , contentModerationDetection
    , cmdModerationLabel
    , cmdTimestamp

    -- * CustomLabel
    , CustomLabel
    , customLabel
    , clConfidence
    , clName
    , clGeometry

    -- * DetectTextFilters
    , DetectTextFilters
    , detectTextFilters
    , dtfRegionsOfInterest
    , dtfWordFilter

    -- * DetectionFilter
    , DetectionFilter
    , detectionFilter
    , dfMinBoundingBoxHeight
    , dfMinBoundingBoxWidth
    , dfMinConfidence

    -- * Emotion
    , Emotion
    , emotion
    , eConfidence
    , eType

    -- * EvaluationResult
    , EvaluationResult
    , evaluationResult
    , erSummary
    , erF1Score

    -- * EyeOpen
    , EyeOpen
    , eyeOpen
    , eoValue
    , eoConfidence

    -- * Eyeglasses
    , Eyeglasses
    , eyeglasses
    , eyeValue
    , eyeConfidence

    -- * Face
    , Face
    , face
    , fFaceId
    , fBoundingBox
    , fExternalImageId
    , fConfidence
    , fImageId

    -- * FaceDetail
    , FaceDetail
    , faceDetail
    , fdAgeRange
    , fdSunglasses
    , fdMouthOpen
    , fdBoundingBox
    , fdEmotions
    , fdEyesOpen
    , fdPose
    , fdConfidence
    , fdGender
    , fdQuality
    , fdEyeglasses
    , fdBeard
    , fdMustache
    , fdSmile
    , fdLandmarks

    -- * FaceDetection
    , FaceDetection
    , faceDetection
    , fdTimestamp
    , fdFace

    -- * FaceMatch
    , FaceMatch
    , faceMatch
    , fmSimilarity
    , fmFace

    -- * FaceRecord
    , FaceRecord
    , faceRecord
    , frFaceDetail
    , frFace

    -- * FaceSearchSettings
    , FaceSearchSettings
    , faceSearchSettings
    , fssFaceMatchThreshold
    , fssCollectionId

    -- * Gender
    , Gender
    , gender
    , gValue
    , gConfidence

    -- * Geometry
    , Geometry
    , geometry
    , gBoundingBox
    , gPolygon

    -- * GroundTruthManifest
    , GroundTruthManifest
    , groundTruthManifest
    , gtmS3Object

    -- * HumanLoopActivationOutput
    , HumanLoopActivationOutput
    , humanLoopActivationOutput
    , hlaoHumanLoopActivationReasons
    , hlaoHumanLoopARN
    , hlaoHumanLoopActivationConditionsEvaluationResults

    -- * HumanLoopConfig
    , HumanLoopConfig
    , humanLoopConfig
    , hlcDataAttributes
    , hlcHumanLoopName
    , hlcFlowDefinitionARN

    -- * HumanLoopDataAttributes
    , HumanLoopDataAttributes
    , humanLoopDataAttributes
    , hldaContentClassifiers

    -- * Image
    , Image
    , image
    , iS3Object
    , iBytes

    -- * ImageQuality
    , ImageQuality
    , imageQuality
    , iqSharpness
    , iqBrightness

    -- * Instance
    , Instance
    , instance'
    , iBoundingBox
    , iConfidence

    -- * KinesisDataStream
    , KinesisDataStream
    , kinesisDataStream
    , kdsARN

    -- * KinesisVideoStream
    , KinesisVideoStream
    , kinesisVideoStream
    , kvsARN

    -- * Label
    , Label
    , label
    , lConfidence
    , lParents
    , lName
    , lInstances

    -- * LabelDetection
    , LabelDetection
    , labelDetection
    , ldLabel
    , ldTimestamp

    -- * Landmark
    , Landmark
    , landmark
    , lType
    , lX
    , lY

    -- * ModerationLabel
    , ModerationLabel
    , moderationLabel
    , mlConfidence
    , mlName
    , mlParentName

    -- * MouthOpen
    , MouthOpen
    , mouthOpen
    , moValue
    , moConfidence

    -- * Mustache
    , Mustache
    , mustache
    , mValue
    , mConfidence

    -- * NotificationChannel
    , NotificationChannel
    , notificationChannel
    , ncSNSTopicARN
    , ncRoleARN

    -- * OutputConfig
    , OutputConfig
    , outputConfig
    , ocS3KeyPrefix
    , ocS3Bucket

    -- * Parent
    , Parent
    , parent
    , pName

    -- * PersonDetail
    , PersonDetail
    , personDetail
    , pdBoundingBox
    , pdIndex
    , pdFace

    -- * PersonDetection
    , PersonDetection
    , personDetection
    , pdPerson
    , pdTimestamp

    -- * PersonMatch
    , PersonMatch
    , personMatch
    , pmFaceMatches
    , pmPerson
    , pmTimestamp

    -- * Point
    , Point
    , point
    , pX
    , pY

    -- * Pose
    , Pose
    , pose
    , pYaw
    , pRoll
    , pPitch

    -- * ProjectDescription
    , ProjectDescription
    , projectDescription
    , pdStatus
    , pdCreationTimestamp
    , pdProjectARN

    -- * ProjectVersionDescription
    , ProjectVersionDescription
    , projectVersionDescription
    , pvdMinInferenceUnits
    , pvdStatus
    , pvdEvaluationResult
    , pvdTestingDataResult
    , pvdStatusMessage
    , pvdCreationTimestamp
    , pvdProjectVersionARN
    , pvdOutputConfig
    , pvdBillableTrainingTimeInSeconds
    , pvdTrainingEndTimestamp
    , pvdTrainingDataResult

    -- * RegionOfInterest
    , RegionOfInterest
    , regionOfInterest
    , roiBoundingBox

    -- * S3Object
    , S3Object
    , s3Object
    , soBucket
    , soName
    , soVersion

    -- * Smile
    , Smile
    , smile
    , smiValue
    , smiConfidence

    -- * StartTextDetectionFilters
    , StartTextDetectionFilters
    , startTextDetectionFilters
    , stdfRegionsOfInterest
    , stdfWordFilter

    -- * StreamProcessor
    , StreamProcessor
    , streamProcessor
    , spStatus
    , spName

    -- * StreamProcessorInput
    , StreamProcessorInput
    , streamProcessorInput
    , spiKinesisVideoStream

    -- * StreamProcessorOutput
    , StreamProcessorOutput
    , streamProcessorOutput
    , spoKinesisDataStream

    -- * StreamProcessorSettings
    , StreamProcessorSettings
    , streamProcessorSettings
    , spsFaceSearch

    -- * Summary
    , Summary
    , summary
    , sS3Object

    -- * Sunglasses
    , Sunglasses
    , sunglasses
    , sValue
    , sConfidence

    -- * TestingData
    , TestingData
    , testingData
    , tdAssets
    , tdAutoCreate

    -- * TestingDataResult
    , TestingDataResult
    , testingDataResult
    , tdrInput
    , tdrOutput

    -- * TextDetection
    , TextDetection
    , textDetection
    , tdDetectedText
    , tdConfidence
    , tdGeometry
    , tdId
    , tdType
    , tdParentId

    -- * TextDetectionResult
    , TextDetectionResult
    , textDetectionResult
    , tdrTextDetection
    , tdrTimestamp

    -- * TrainingData
    , TrainingData
    , trainingData
    , tAssets

    -- * TrainingDataResult
    , TrainingDataResult
    , trainingDataResult
    , tInput
    , tOutput

    -- * UnindexedFace
    , UnindexedFace
    , unindexedFace
    , ufReasons
    , ufFaceDetail

    -- * Video
    , Video
    , video
    , vS3Object

    -- * VideoMetadata
    , VideoMetadata
    , videoMetadata
    , vmFrameRate
    , vmFormat
    , vmCodec
    , vmFrameHeight
    , vmDurationMillis
    , vmFrameWidth
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.Rekognition.Types.Attribute
import Network.AWS.Rekognition.Types.CelebrityRecognitionSortBy
import Network.AWS.Rekognition.Types.ContentClassifier
import Network.AWS.Rekognition.Types.ContentModerationSortBy
import Network.AWS.Rekognition.Types.EmotionName
import Network.AWS.Rekognition.Types.FaceAttributes
import Network.AWS.Rekognition.Types.FaceSearchSortBy
import Network.AWS.Rekognition.Types.GenderType
import Network.AWS.Rekognition.Types.LabelDetectionSortBy
import Network.AWS.Rekognition.Types.LandmarkType
import Network.AWS.Rekognition.Types.OrientationCorrection
import Network.AWS.Rekognition.Types.PersonTrackingSortBy
import Network.AWS.Rekognition.Types.ProjectStatus
import Network.AWS.Rekognition.Types.ProjectVersionStatus
import Network.AWS.Rekognition.Types.QualityFilter
import Network.AWS.Rekognition.Types.Reason
import Network.AWS.Rekognition.Types.StreamProcessorStatus
import Network.AWS.Rekognition.Types.TextTypes
import Network.AWS.Rekognition.Types.VideoJobStatus
import Network.AWS.Rekognition.Types.AgeRange
import Network.AWS.Rekognition.Types.Asset
import Network.AWS.Rekognition.Types.Beard
import Network.AWS.Rekognition.Types.BoundingBox
import Network.AWS.Rekognition.Types.Celebrity
import Network.AWS.Rekognition.Types.CelebrityDetail
import Network.AWS.Rekognition.Types.CelebrityRecognition
import Network.AWS.Rekognition.Types.CompareFacesMatch
import Network.AWS.Rekognition.Types.ComparedFace
import Network.AWS.Rekognition.Types.ComparedSourceImageFace
import Network.AWS.Rekognition.Types.ContentModerationDetection
import Network.AWS.Rekognition.Types.CustomLabel
import Network.AWS.Rekognition.Types.DetectTextFilters
import Network.AWS.Rekognition.Types.DetectionFilter
import Network.AWS.Rekognition.Types.Emotion
import Network.AWS.Rekognition.Types.EvaluationResult
import Network.AWS.Rekognition.Types.EyeOpen
import Network.AWS.Rekognition.Types.Eyeglasses
import Network.AWS.Rekognition.Types.Face
import Network.AWS.Rekognition.Types.FaceDetail
import Network.AWS.Rekognition.Types.FaceDetection
import Network.AWS.Rekognition.Types.FaceMatch
import Network.AWS.Rekognition.Types.FaceRecord
import Network.AWS.Rekognition.Types.FaceSearchSettings
import Network.AWS.Rekognition.Types.Gender
import Network.AWS.Rekognition.Types.Geometry
import Network.AWS.Rekognition.Types.GroundTruthManifest
import Network.AWS.Rekognition.Types.HumanLoopActivationOutput
import Network.AWS.Rekognition.Types.HumanLoopConfig
import Network.AWS.Rekognition.Types.HumanLoopDataAttributes
import Network.AWS.Rekognition.Types.Image
import Network.AWS.Rekognition.Types.ImageQuality
import Network.AWS.Rekognition.Types.Instance
import Network.AWS.Rekognition.Types.KinesisDataStream
import Network.AWS.Rekognition.Types.KinesisVideoStream
import Network.AWS.Rekognition.Types.Label
import Network.AWS.Rekognition.Types.LabelDetection
import Network.AWS.Rekognition.Types.Landmark
import Network.AWS.Rekognition.Types.ModerationLabel
import Network.AWS.Rekognition.Types.MouthOpen
import Network.AWS.Rekognition.Types.Mustache
import Network.AWS.Rekognition.Types.NotificationChannel
import Network.AWS.Rekognition.Types.OutputConfig
import Network.AWS.Rekognition.Types.Parent
import Network.AWS.Rekognition.Types.PersonDetail
import Network.AWS.Rekognition.Types.PersonDetection
import Network.AWS.Rekognition.Types.PersonMatch
import Network.AWS.Rekognition.Types.Point
import Network.AWS.Rekognition.Types.Pose
import Network.AWS.Rekognition.Types.ProjectDescription
import Network.AWS.Rekognition.Types.ProjectVersionDescription
import Network.AWS.Rekognition.Types.RegionOfInterest
import Network.AWS.Rekognition.Types.S3Object
import Network.AWS.Rekognition.Types.Smile
import Network.AWS.Rekognition.Types.StartTextDetectionFilters
import Network.AWS.Rekognition.Types.StreamProcessor
import Network.AWS.Rekognition.Types.StreamProcessorInput
import Network.AWS.Rekognition.Types.StreamProcessorOutput
import Network.AWS.Rekognition.Types.StreamProcessorSettings
import Network.AWS.Rekognition.Types.Summary
import Network.AWS.Rekognition.Types.Sunglasses
import Network.AWS.Rekognition.Types.TestingData
import Network.AWS.Rekognition.Types.TestingDataResult
import Network.AWS.Rekognition.Types.TextDetection
import Network.AWS.Rekognition.Types.TextDetectionResult
import Network.AWS.Rekognition.Types.TrainingData
import Network.AWS.Rekognition.Types.TrainingDataResult
import Network.AWS.Rekognition.Types.UnindexedFace
import Network.AWS.Rekognition.Types.Video
import Network.AWS.Rekognition.Types.VideoMetadata

-- | API version @2016-06-27@ of the Amazon Rekognition SDK configuration.
rekognition :: Service
rekognition
  = Service{_svcAbbrev = "Rekognition",
            _svcSigner = v4, _svcPrefix = "rekognition",
            _svcVersion = "2016-06-27",
            _svcEndpoint = defaultEndpoint rekognition,
            _svcTimeout = Just 70, _svcCheck = statusSuccess,
            _svcError = parseJSONError "Rekognition",
            _svcRetry = retry}
  where retry
          = Exponential{_retryBase = 5.0e-2, _retryGrowth = 2,
                        _retryAttempts = 5, _retryCheck = check}
        check e
          | has (hasCode "ThrottledException" . hasStatus 400)
              e
            = Just "throttled_exception"
          | has (hasStatus 429) e = Just "too_many_requests"
          | has (hasCode "ThrottlingException" . hasStatus 400)
              e
            = Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e =
            Just "throttling"
          | has
              (hasCode "ProvisionedThroughputExceededException" .
                 hasStatus 400)
              e
            = Just "throughput_exceeded"
          | has (hasStatus 504) e = Just "gateway_timeout"
          | has
              (hasCode "RequestThrottledException" . hasStatus 400)
              e
            = Just "request_throttled_exception"
          | has (hasStatus 502) e = Just "bad_gateway"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | A @ClientRequestToken@ input parameter was reused with an operation, but at least one of the other input parameters is different from the previous call to the operation.
--
--
_IdempotentParameterMismatchException :: AsError a => Getting (First ServiceError) a ServiceError
_IdempotentParameterMismatchException
  = _MatchServiceError rekognition
      "IdempotentParameterMismatchException"

-- | The provided image format is not supported. 
--
--
_InvalidImageFormatException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidImageFormatException
  = _MatchServiceError rekognition
      "InvalidImageFormatException"

-- | Input parameter violated a constraint. Validate your parameter before calling the API operation again.
--
--
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException
  = _MatchServiceError rekognition
      "InvalidParameterException"

-- | You are not authorized to perform the action.
--
--
_AccessDeniedException :: AsError a => Getting (First ServiceError) a ServiceError
_AccessDeniedException
  = _MatchServiceError rekognition
      "AccessDeniedException"

-- | The input image size exceeds the allowed limit. For more information, see Limits in Amazon Rekognition in the Amazon Rekognition Developer Guide. 
--
--
_ImageTooLargeException :: AsError a => Getting (First ServiceError) a ServiceError
_ImageTooLargeException
  = _MatchServiceError rekognition
      "ImageTooLargeException"

-- | Pagination token in the request is not valid.
--
--
_InvalidPaginationTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidPaginationTokenException
  = _MatchServiceError rekognition
      "InvalidPaginationTokenException"

-- | The number of in-progress human reviews you have has exceeded the number allowed.
--
--
_HumanLoopQuotaExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_HumanLoopQuotaExceededException
  = _MatchServiceError rekognition
      "HumanLoopQuotaExceededException"

-- | The number of requests exceeded your throughput limit. If you want to increase this limit, contact Amazon Rekognition.
--
--
_ProvisionedThroughputExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_ProvisionedThroughputExceededException
  = _MatchServiceError rekognition
      "ProvisionedThroughputExceededException"

-- | The collection specified in the request cannot be found.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException
  = _MatchServiceError rekognition
      "ResourceNotFoundException"

-- | Amazon Rekognition is unable to access the S3 object specified in the request.
--
--
_InvalidS3ObjectException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidS3ObjectException
  = _MatchServiceError rekognition
      "InvalidS3ObjectException"

-- | The requested resource isn't ready. For example, this exception occurs when you call @DetectCustomLabels@ with a model version that isn't deployed. 
--
--
_ResourceNotReadyException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotReadyException
  = _MatchServiceError rekognition
      "ResourceNotReadyException"

-- | A collection with the specified ID already exists.
--
--
_ResourceAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceAlreadyExistsException
  = _MatchServiceError rekognition
      "ResourceAlreadyExistsException"

-- | Amazon Rekognition experienced a service issue. Try your call again.
--
--
_InternalServerError :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerError
  = _MatchServiceError rekognition
      "InternalServerError"

-- | Amazon Rekognition is temporarily unable to process the request. Try your call again.
--
--
_ThrottlingException :: AsError a => Getting (First ServiceError) a ServiceError
_ThrottlingException
  = _MatchServiceError rekognition
      "ThrottlingException"

-- | The file size or duration of the supplied media is too large. The maximum file size is 10GB. The maximum duration is 6 hours. 
--
--
_VideoTooLargeException :: AsError a => Getting (First ServiceError) a ServiceError
_VideoTooLargeException
  = _MatchServiceError rekognition
      "VideoTooLargeException"

-- | An Amazon Rekognition service limit was exceeded. For example, if you start too many Amazon Rekognition Video jobs concurrently, calls to start operations (@StartLabelDetection@ , for example) will raise a @LimitExceededException@ exception (HTTP status code: 400) until the number of concurrently running jobs is below the Amazon Rekognition service limit. 
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException
  = _MatchServiceError rekognition
      "LimitExceededException"

-- | 
--
--
_ResourceInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceInUseException
  = _MatchServiceError rekognition
      "ResourceInUseException"
