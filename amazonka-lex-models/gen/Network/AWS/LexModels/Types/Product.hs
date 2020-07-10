{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.Product (
    module Network.AWS.LexModels.Types.BotAliasMetadata,
    module Network.AWS.LexModels.Types.BotChannelAssociation,
    module Network.AWS.LexModels.Types.BotMetadata,
    module Network.AWS.LexModels.Types.BuiltinIntentMetadata,
    module Network.AWS.LexModels.Types.BuiltinIntentSlot,
    module Network.AWS.LexModels.Types.BuiltinSlotTypeMetadata,
    module Network.AWS.LexModels.Types.CodeHook,
    module Network.AWS.LexModels.Types.ConversationLogsRequest,
    module Network.AWS.LexModels.Types.ConversationLogsResponse,
    module Network.AWS.LexModels.Types.EnumerationValue,
    module Network.AWS.LexModels.Types.FollowUpPrompt,
    module Network.AWS.LexModels.Types.FulfillmentActivity,
    module Network.AWS.LexModels.Types.Intent,
    module Network.AWS.LexModels.Types.IntentMetadata,
    module Network.AWS.LexModels.Types.LogSettingsRequest,
    module Network.AWS.LexModels.Types.LogSettingsResponse,
    module Network.AWS.LexModels.Types.Message,
    module Network.AWS.LexModels.Types.Prompt,
    module Network.AWS.LexModels.Types.Slot,
    module Network.AWS.LexModels.Types.SlotTypeConfiguration,
    module Network.AWS.LexModels.Types.SlotTypeMetadata,
    module Network.AWS.LexModels.Types.SlotTypeRegexConfiguration,
    module Network.AWS.LexModels.Types.Statement,
    module Network.AWS.LexModels.Types.Tag,
    module Network.AWS.LexModels.Types.UtteranceData,
    module Network.AWS.LexModels.Types.UtteranceList
  ) where

import Network.AWS.Lens
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
import Network.AWS.Prelude
