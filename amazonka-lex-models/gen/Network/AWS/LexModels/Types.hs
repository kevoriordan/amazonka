{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types
    (
    -- * Service Configuration
      lexModels

    -- * Errors
    , _PreconditionFailedException
    , _InternalFailureException
    , _BadRequestException
    , _NotFoundException
    , _LimitExceededException
    , _ConflictException
    , _ResourceInUseException

    -- * ChannelStatus
    , ChannelStatus (..)

    -- * ChannelType
    , ChannelType (..)

    -- * ContentType
    , ContentType (..)

    -- * Destination
    , Destination (..)

    -- * ExportStatus
    , ExportStatus (..)

    -- * ExportType
    , ExportType (..)

    -- * FulfillmentActivityType
    , FulfillmentActivityType (..)

    -- * ImportStatus
    , ImportStatus (..)

    -- * LexStatus
    , LexStatus (..)

    -- * Locale
    , Locale (..)

    -- * LogType
    , LogType (..)

    -- * MergeStrategy
    , MergeStrategy (..)

    -- * ObfuscationSetting
    , ObfuscationSetting (..)

    -- * ProcessBehavior
    , ProcessBehavior (..)

    -- * ResourceType
    , ResourceType (..)

    -- * SlotConstraint
    , SlotConstraint (..)

    -- * SlotValueSelectionStrategy
    , SlotValueSelectionStrategy (..)

    -- * StatusType
    , StatusType (..)

    -- * BotAliasMetadata
    , BotAliasMetadata
    , botAliasMetadata
    , bamChecksum
    , bamBotVersion
    , bamBotName
    , bamCreatedDate
    , bamName
    , bamConversationLogs
    , bamLastUpdatedDate
    , bamDescription

    -- * BotChannelAssociation
    , BotChannelAssociation
    , botChannelAssociation
    , bcaFailureReason
    , bcaStatus
    , bcaBotAlias
    , bcaBotName
    , bcaBotConfiguration
    , bcaCreatedDate
    , bcaName
    , bcaType
    , bcaDescription

    -- * BotMetadata
    , BotMetadata
    , botMetadata
    , bmStatus
    , bmCreatedDate
    , bmName
    , bmVersion
    , bmLastUpdatedDate
    , bmDescription

    -- * BuiltinIntentMetadata
    , BuiltinIntentMetadata
    , builtinIntentMetadata
    , bimSignature
    , bimSupportedLocales

    -- * BuiltinIntentSlot
    , BuiltinIntentSlot
    , builtinIntentSlot
    , bisName

    -- * BuiltinSlotTypeMetadata
    , BuiltinSlotTypeMetadata
    , builtinSlotTypeMetadata
    , bstmSignature
    , bstmSupportedLocales

    -- * CodeHook
    , CodeHook
    , codeHook
    , chUri
    , chMessageVersion

    -- * ConversationLogsRequest
    , ConversationLogsRequest
    , conversationLogsRequest
    , clrLogSettings
    , clrIamRoleARN

    -- * ConversationLogsResponse
    , ConversationLogsResponse
    , conversationLogsResponse
    , clIamRoleARN
    , clLogSettings

    -- * EnumerationValue
    , EnumerationValue
    , enumerationValue
    , evSynonyms
    , evValue

    -- * FollowUpPrompt
    , FollowUpPrompt
    , followUpPrompt
    , fupPrompt
    , fupRejectionStatement

    -- * FulfillmentActivity
    , FulfillmentActivity
    , fulfillmentActivity
    , faCodeHook
    , faType

    -- * Intent
    , Intent
    , intent
    , iIntentName
    , iIntentVersion

    -- * IntentMetadata
    , IntentMetadata
    , intentMetadata
    , imCreatedDate
    , imName
    , imVersion
    , imLastUpdatedDate
    , imDescription

    -- * LogSettingsRequest
    , LogSettingsRequest
    , logSettingsRequest
    , lsrKmsKeyARN
    , lsrLogType
    , lsrDestination
    , lsrResourceARN

    -- * LogSettingsResponse
    , LogSettingsResponse
    , logSettingsResponse
    , lsDestination
    , lsKmsKeyARN
    , lsLogType
    , lsResourceARN
    , lsResourcePrefix

    -- * Message
    , Message
    , message
    , mGroupNumber
    , mContentType
    , mContent

    -- * Prompt
    , Prompt
    , prompt
    , pResponseCard
    , pMessages
    , pMaxAttempts

    -- * Slot
    , Slot
    , slot
    , sSlotType
    , sValueElicitationPrompt
    , sResponseCard
    , sPriority
    , sObfuscationSetting
    , sSlotTypeVersion
    , sSampleUtterances
    , sDescription
    , sName
    , sSlotConstraint

    -- * SlotTypeConfiguration
    , SlotTypeConfiguration
    , slotTypeConfiguration
    , stcRegexConfiguration

    -- * SlotTypeMetadata
    , SlotTypeMetadata
    , slotTypeMetadata
    , stmCreatedDate
    , stmName
    , stmVersion
    , stmLastUpdatedDate
    , stmDescription

    -- * SlotTypeRegexConfiguration
    , SlotTypeRegexConfiguration
    , slotTypeRegexConfiguration
    , strcPattern

    -- * Statement
    , Statement
    , statement
    , staResponseCard
    , staMessages

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- * UtteranceData
    , UtteranceData
    , utteranceData
    , udFirstUtteredDate
    , udCount
    , udUtteranceString
    , udLastUtteredDate
    , udDistinctUsers

    -- * UtteranceList
    , UtteranceList
    , utteranceList
    , ulBotVersion
    , ulUtterances
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.LexModels.Types.ChannelStatus
import Network.AWS.LexModels.Types.ChannelType
import Network.AWS.LexModels.Types.ContentType
import Network.AWS.LexModels.Types.Destination
import Network.AWS.LexModels.Types.ExportStatus
import Network.AWS.LexModels.Types.ExportType
import Network.AWS.LexModels.Types.FulfillmentActivityType
import Network.AWS.LexModels.Types.ImportStatus
import Network.AWS.LexModels.Types.LexStatus
import Network.AWS.LexModels.Types.Locale
import Network.AWS.LexModels.Types.LogType
import Network.AWS.LexModels.Types.MergeStrategy
import Network.AWS.LexModels.Types.ObfuscationSetting
import Network.AWS.LexModels.Types.ProcessBehavior
import Network.AWS.LexModels.Types.ResourceType
import Network.AWS.LexModels.Types.SlotConstraint
import Network.AWS.LexModels.Types.SlotValueSelectionStrategy
import Network.AWS.LexModels.Types.StatusType
import Network.AWS.LexModels.Types.BotAliasMetadata
import Network.AWS.LexModels.Types.BotChannelAssociation
import Network.AWS.LexModels.Types.BotMetadata
import Network.AWS.LexModels.Types.BuiltinIntentMetadata
import Network.AWS.LexModels.Types.BuiltinIntentSlot
import Network.AWS.LexModels.Types.BuiltinSlotTypeMetadata
import Network.AWS.LexModels.Types.CodeHook
import Network.AWS.LexModels.Types.ConversationLogsRequest
import Network.AWS.LexModels.Types.ConversationLogsResponse
import Network.AWS.LexModels.Types.EnumerationValue
import Network.AWS.LexModels.Types.FollowUpPrompt
import Network.AWS.LexModels.Types.FulfillmentActivity
import Network.AWS.LexModels.Types.Intent
import Network.AWS.LexModels.Types.IntentMetadata
import Network.AWS.LexModels.Types.LogSettingsRequest
import Network.AWS.LexModels.Types.LogSettingsResponse
import Network.AWS.LexModels.Types.Message
import Network.AWS.LexModels.Types.Prompt
import Network.AWS.LexModels.Types.Slot
import Network.AWS.LexModels.Types.SlotTypeConfiguration
import Network.AWS.LexModels.Types.SlotTypeMetadata
import Network.AWS.LexModels.Types.SlotTypeRegexConfiguration
import Network.AWS.LexModels.Types.Statement
import Network.AWS.LexModels.Types.Tag
import Network.AWS.LexModels.Types.UtteranceData
import Network.AWS.LexModels.Types.UtteranceList

-- | API version @2017-04-19@ of the Amazon Lex Model Building Service SDK configuration.
lexModels :: Service
lexModels
  = Service{_svcAbbrev = "LexModels", _svcSigner = v4,
            _svcPrefix = "models.lex",
            _svcVersion = "2017-04-19",
            _svcEndpoint = defaultEndpoint lexModels,
            _svcTimeout = Just 70, _svcCheck = statusSuccess,
            _svcError = parseJSONError "LexModels",
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

-- | The checksum of the resource that you are trying to change does not match the checksum in the request. Check the resource's checksum and try again.
--
--
_PreconditionFailedException :: AsError a => Getting (First ServiceError) a ServiceError
_PreconditionFailedException
  = _MatchServiceError lexModels
      "PreconditionFailedException"
      . hasStatus 412

-- | An internal Amazon Lex error occurred. Try your request again.
--
--
_InternalFailureException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalFailureException
  = _MatchServiceError lexModels
      "InternalFailureException"
      . hasStatus 500

-- | The request is not well formed. For example, a value is invalid or a required field is missing. Check the field values, and try again.
--
--
_BadRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_BadRequestException
  = _MatchServiceError lexModels "BadRequestException"
      . hasStatus 400

-- | The resource specified in the request was not found. Check the resource and try again.
--
--
_NotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_NotFoundException
  = _MatchServiceError lexModels "NotFoundException" .
      hasStatus 404

-- | The request exceeded a limit. Try your request again.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException
  = _MatchServiceError lexModels
      "LimitExceededException"
      . hasStatus 429

-- | There was a conflict processing the request. Try your request again. 
--
--
_ConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_ConflictException
  = _MatchServiceError lexModels "ConflictException" .
      hasStatus 409

-- | The resource that you are attempting to delete is referred to by another resource. Use this information to remove references to the resource that you are trying to delete.
--
--
-- The body of the exception contains a JSON object that describes the resource.
--
-- @{ "resourceType": BOT | BOTALIAS | BOTCHANNEL | INTENT,@ 
--
-- @"resourceReference": {@ 
--
-- @"name": /string/ , "version": /string/ } }@ 
--
_ResourceInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceInUseException
  = _MatchServiceError lexModels
      "ResourceInUseException"
      . hasStatus 400
