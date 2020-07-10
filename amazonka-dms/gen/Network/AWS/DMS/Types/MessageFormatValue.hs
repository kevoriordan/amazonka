{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.MessageFormatValue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.MessageFormatValue (
  MessageFormatValue (
    ..
    , JSON
    , JSONUnformatted
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MessageFormatValue = MessageFormatValue' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern JSON :: MessageFormatValue
pattern JSON = MessageFormatValue' "json"

pattern JSONUnformatted :: MessageFormatValue
pattern JSONUnformatted = MessageFormatValue' "json-unformatted"

{-# COMPLETE
  JSON,
  JSONUnformatted,
  MessageFormatValue' #-}

instance FromText MessageFormatValue where
    parser = (MessageFormatValue' . mk) <$> takeText

instance ToText MessageFormatValue where
    toText (MessageFormatValue' ci) = original ci

-- | Represents an enum of /known/ $MessageFormatValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MessageFormatValue where
    toEnum i = case i of
        0 -> JSON
        1 -> JSONUnformatted
        _ -> (error . showText) $ "Unknown index for MessageFormatValue: " <> toText i
    fromEnum x = case x of
        JSON -> 0
        JSONUnformatted -> 1
        MessageFormatValue' name -> (error . showText) $ "Unknown MessageFormatValue: " <> original name

-- | Represents the bounds of /known/ $MessageFormatValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MessageFormatValue where
    minBound = JSON
    maxBound = JSONUnformatted

instance Hashable     MessageFormatValue
instance NFData       MessageFormatValue
instance ToByteString MessageFormatValue
instance ToQuery      MessageFormatValue
instance ToHeader     MessageFormatValue

instance ToJSON MessageFormatValue where
    toJSON = toJSONText

instance FromJSON MessageFormatValue where
    parseJSON = parseJSONText "MessageFormatValue"
