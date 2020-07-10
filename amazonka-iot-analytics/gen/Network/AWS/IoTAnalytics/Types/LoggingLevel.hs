{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.LoggingLevel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.LoggingLevel (
  LoggingLevel (
    ..
    , Error'
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LoggingLevel = LoggingLevel' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Error' :: LoggingLevel
pattern Error' = LoggingLevel' "ERROR"

{-# COMPLETE
  Error',
  LoggingLevel' #-}

instance FromText LoggingLevel where
    parser = (LoggingLevel' . mk) <$> takeText

instance ToText LoggingLevel where
    toText (LoggingLevel' ci) = original ci

-- | Represents an enum of /known/ $LoggingLevel.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LoggingLevel where
    toEnum i = case i of
        0 -> Error'
        _ -> (error . showText) $ "Unknown index for LoggingLevel: " <> toText i
    fromEnum x = case x of
        Error' -> 0
        LoggingLevel' name -> (error . showText) $ "Unknown LoggingLevel: " <> original name

-- | Represents the bounds of /known/ $LoggingLevel.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LoggingLevel where
    minBound = Error'
    maxBound = Error'

instance Hashable     LoggingLevel
instance NFData       LoggingLevel
instance ToByteString LoggingLevel
instance ToQuery      LoggingLevel
instance ToHeader     LoggingLevel

instance ToJSON LoggingLevel where
    toJSON = toJSONText

instance FromJSON LoggingLevel where
    parseJSON = parseJSONText "LoggingLevel"
