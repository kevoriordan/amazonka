{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.MessageAttribute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SQS.Types.MessageAttribute (
  MessageAttribute (
    ..
    , All
    , ApproximateFirstReceiveTimestamp
    , ApproximateReceiveCount
    , SenderId
    , SentTimestamp
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MessageAttribute = MessageAttribute' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern All :: MessageAttribute
pattern All = MessageAttribute' "All"

pattern ApproximateFirstReceiveTimestamp :: MessageAttribute
pattern ApproximateFirstReceiveTimestamp = MessageAttribute' "ApproximateFirstReceiveTimestamp"

pattern ApproximateReceiveCount :: MessageAttribute
pattern ApproximateReceiveCount = MessageAttribute' "ApproximateReceiveCount"

pattern SenderId :: MessageAttribute
pattern SenderId = MessageAttribute' "SenderId"

pattern SentTimestamp :: MessageAttribute
pattern SentTimestamp = MessageAttribute' "SentTimestamp"

{-# COMPLETE
  All,
  ApproximateFirstReceiveTimestamp,
  ApproximateReceiveCount,
  SenderId,
  SentTimestamp,
  MessageAttribute' #-}

instance FromText MessageAttribute where
    parser = (MessageAttribute' . mk) <$> takeText

instance ToText MessageAttribute where
    toText (MessageAttribute' ci) = original ci

-- | Represents an enum of /known/ $MessageAttribute.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MessageAttribute where
    toEnum i = case i of
        0 -> All
        1 -> ApproximateFirstReceiveTimestamp
        2 -> ApproximateReceiveCount
        3 -> SenderId
        4 -> SentTimestamp
        _ -> (error . showText) $ "Unknown index for MessageAttribute: " <> toText i
    fromEnum x = case x of
        All -> 0
        ApproximateFirstReceiveTimestamp -> 1
        ApproximateReceiveCount -> 2
        SenderId -> 3
        SentTimestamp -> 4
        MessageAttribute' name -> (error . showText) $ "Unknown MessageAttribute: " <> original name

-- | Represents the bounds of /known/ $MessageAttribute.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MessageAttribute where
    minBound = All
    maxBound = SentTimestamp

instance Hashable     MessageAttribute
instance NFData       MessageAttribute
instance ToByteString MessageAttribute
instance ToQuery      MessageAttribute
instance ToHeader     MessageAttribute

instance FromXML MessageAttribute where
    parseXML = parseXMLText "MessageAttribute"
