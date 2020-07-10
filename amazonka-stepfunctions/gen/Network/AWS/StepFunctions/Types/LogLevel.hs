{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.LogLevel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.LogLevel (
  LogLevel (
    ..
    , All
    , Error'
    , Fatal
    , Off
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LogLevel = LogLevel' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern All :: LogLevel
pattern All = LogLevel' "ALL"

pattern Error' :: LogLevel
pattern Error' = LogLevel' "ERROR"

pattern Fatal :: LogLevel
pattern Fatal = LogLevel' "FATAL"

pattern Off :: LogLevel
pattern Off = LogLevel' "OFF"

{-# COMPLETE
  All,
  Error',
  Fatal,
  Off,
  LogLevel' #-}

instance FromText LogLevel where
    parser = (LogLevel' . mk) <$> takeText

instance ToText LogLevel where
    toText (LogLevel' ci) = original ci

-- | Represents an enum of /known/ $LogLevel.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LogLevel where
    toEnum i = case i of
        0 -> All
        1 -> Error'
        2 -> Fatal
        3 -> Off
        _ -> (error . showText) $ "Unknown index for LogLevel: " <> toText i
    fromEnum x = case x of
        All -> 0
        Error' -> 1
        Fatal -> 2
        Off -> 3
        LogLevel' name -> (error . showText) $ "Unknown LogLevel: " <> original name

-- | Represents the bounds of /known/ $LogLevel.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LogLevel where
    minBound = All
    maxBound = Off

instance Hashable     LogLevel
instance NFData       LogLevel
instance ToByteString LogLevel
instance ToQuery      LogLevel
instance ToHeader     LogLevel

instance ToJSON LogLevel where
    toJSON = toJSONText

instance FromJSON LogLevel where
    parseJSON = parseJSONText "LogLevel"
