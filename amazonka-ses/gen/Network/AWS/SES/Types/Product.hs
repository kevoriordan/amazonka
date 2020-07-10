{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.Product (
    module Network.AWS.SES.Types.AddHeaderAction,
    module Network.AWS.SES.Types.Body,
    module Network.AWS.SES.Types.BounceAction,
    module Network.AWS.SES.Types.BouncedRecipientInfo,
    module Network.AWS.SES.Types.BulkEmailDestination,
    module Network.AWS.SES.Types.BulkEmailDestinationStatus,
    module Network.AWS.SES.Types.CloudWatchDestination,
    module Network.AWS.SES.Types.CloudWatchDimensionConfiguration,
    module Network.AWS.SES.Types.ConfigurationSet,
    module Network.AWS.SES.Types.Content,
    module Network.AWS.SES.Types.CustomVerificationEmailTemplate,
    module Network.AWS.SES.Types.DeliveryOptions,
    module Network.AWS.SES.Types.Destination,
    module Network.AWS.SES.Types.EventDestination,
    module Network.AWS.SES.Types.ExtensionField,
    module Network.AWS.SES.Types.IdentityDkimAttributes,
    module Network.AWS.SES.Types.IdentityMailFromDomainAttributes,
    module Network.AWS.SES.Types.IdentityNotificationAttributes,
    module Network.AWS.SES.Types.IdentityVerificationAttributes,
    module Network.AWS.SES.Types.KinesisFirehoseDestination,
    module Network.AWS.SES.Types.LambdaAction,
    module Network.AWS.SES.Types.Message,
    module Network.AWS.SES.Types.MessageDsn,
    module Network.AWS.SES.Types.MessageTag,
    module Network.AWS.SES.Types.RawMessage,
    module Network.AWS.SES.Types.ReceiptAction,
    module Network.AWS.SES.Types.ReceiptFilter,
    module Network.AWS.SES.Types.ReceiptIPFilter,
    module Network.AWS.SES.Types.ReceiptRule,
    module Network.AWS.SES.Types.ReceiptRuleSetMetadata,
    module Network.AWS.SES.Types.RecipientDsnFields,
    module Network.AWS.SES.Types.ReputationOptions,
    module Network.AWS.SES.Types.S3Action,
    module Network.AWS.SES.Types.SNSAction,
    module Network.AWS.SES.Types.SNSDestination,
    module Network.AWS.SES.Types.SendDataPoint,
    module Network.AWS.SES.Types.StopAction,
    module Network.AWS.SES.Types.Template,
    module Network.AWS.SES.Types.TemplateMetadata,
    module Network.AWS.SES.Types.TrackingOptions,
    module Network.AWS.SES.Types.WorkmailAction
  ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SES.Types.AddHeaderAction
import Network.AWS.SES.Types.Body
import Network.AWS.SES.Types.BounceAction
import Network.AWS.SES.Types.BouncedRecipientInfo
import Network.AWS.SES.Types.BulkEmailDestination
import Network.AWS.SES.Types.BulkEmailDestinationStatus
import Network.AWS.SES.Types.CloudWatchDestination
import Network.AWS.SES.Types.CloudWatchDimensionConfiguration
import Network.AWS.SES.Types.ConfigurationSet
import Network.AWS.SES.Types.Content
import Network.AWS.SES.Types.CustomVerificationEmailTemplate
import Network.AWS.SES.Types.DeliveryOptions
import Network.AWS.SES.Types.Destination
import Network.AWS.SES.Types.EventDestination
import Network.AWS.SES.Types.ExtensionField
import Network.AWS.SES.Types.IdentityDkimAttributes
import Network.AWS.SES.Types.IdentityMailFromDomainAttributes
import Network.AWS.SES.Types.IdentityNotificationAttributes
import Network.AWS.SES.Types.IdentityVerificationAttributes
import Network.AWS.SES.Types.KinesisFirehoseDestination
import Network.AWS.SES.Types.LambdaAction
import Network.AWS.SES.Types.Message
import Network.AWS.SES.Types.MessageDsn
import Network.AWS.SES.Types.MessageTag
import Network.AWS.SES.Types.RawMessage
import Network.AWS.SES.Types.ReceiptAction
import Network.AWS.SES.Types.ReceiptFilter
import Network.AWS.SES.Types.ReceiptIPFilter
import Network.AWS.SES.Types.ReceiptRule
import Network.AWS.SES.Types.ReceiptRuleSetMetadata
import Network.AWS.SES.Types.RecipientDsnFields
import Network.AWS.SES.Types.ReputationOptions
import Network.AWS.SES.Types.S3Action
import Network.AWS.SES.Types.SNSAction
import Network.AWS.SES.Types.SNSDestination
import Network.AWS.SES.Types.SendDataPoint
import Network.AWS.SES.Types.StopAction
import Network.AWS.SES.Types.Template
import Network.AWS.SES.Types.TemplateMetadata
import Network.AWS.SES.Types.TrackingOptions
import Network.AWS.SES.Types.WorkmailAction
