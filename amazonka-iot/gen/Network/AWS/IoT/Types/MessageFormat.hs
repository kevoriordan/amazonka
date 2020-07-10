{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.MessageFormat
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.MessageFormat (
  MessageFormat (
    ..
    , JSON
    , Raw
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MessageFormat = MessageFormat' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern JSON :: MessageFormat
pattern JSON = MessageFormat' "JSON"

pattern Raw :: MessageFormat
pattern Raw = MessageFormat' "RAW"

{-# COMPLETE
  JSON,
  Raw,
  MessageFormat' #-}

instance FromText MessageFormat where
    parser = (MessageFormat' . mk) <$> takeText

instance ToText MessageFormat where
    toText (MessageFormat' ci) = original ci

-- | Represents an enum of /known/ $MessageFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MessageFormat where
    toEnum i = case i of
        0 -> JSON
        1 -> Raw
        _ -> (error . showText) $ "Unknown index for MessageFormat: " <> toText i
    fromEnum x = case x of
        JSON -> 0
        Raw -> 1
        MessageFormat' name -> (error . showText) $ "Unknown MessageFormat: " <> original name

-- | Represents the bounds of /known/ $MessageFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MessageFormat where
    minBound = JSON
    maxBound = Raw

instance Hashable     MessageFormat
instance NFData       MessageFormat
instance ToByteString MessageFormat
instance ToQuery      MessageFormat
instance ToHeader     MessageFormat

instance ToJSON MessageFormat where
    toJSON = toJSONText

instance FromJSON MessageFormat where
    parseJSON = parseJSONText "MessageFormat"
