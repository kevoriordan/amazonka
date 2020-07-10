{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SQS.Types.Product (
    module Network.AWS.SQS.Types.BatchResultErrorEntry,
    module Network.AWS.SQS.Types.ChangeMessageVisibilityBatchRequestEntry,
    module Network.AWS.SQS.Types.ChangeMessageVisibilityBatchResultEntry,
    module Network.AWS.SQS.Types.DeleteMessageBatchRequestEntry,
    module Network.AWS.SQS.Types.DeleteMessageBatchResultEntry,
    module Network.AWS.SQS.Types.Message,
    module Network.AWS.SQS.Types.MessageAttributeValue,
    module Network.AWS.SQS.Types.MessageSystemAttributeValue,
    module Network.AWS.SQS.Types.SendMessageBatchRequestEntry,
    module Network.AWS.SQS.Types.SendMessageBatchResultEntry
  ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SQS.Types.BatchResultErrorEntry
import Network.AWS.SQS.Types.ChangeMessageVisibilityBatchRequestEntry
import Network.AWS.SQS.Types.ChangeMessageVisibilityBatchResultEntry
import Network.AWS.SQS.Types.DeleteMessageBatchRequestEntry
import Network.AWS.SQS.Types.DeleteMessageBatchResultEntry
import Network.AWS.SQS.Types.Message
import Network.AWS.SQS.Types.MessageAttributeValue
import Network.AWS.SQS.Types.MessageSystemAttributeValue
import Network.AWS.SQS.Types.SendMessageBatchRequestEntry
import Network.AWS.SQS.Types.SendMessageBatchResultEntry
