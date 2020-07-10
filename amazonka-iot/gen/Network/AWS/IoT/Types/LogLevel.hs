{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.LogLevel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.LogLevel (
  LogLevel (
    ..
    , Debug
    , Disabled
    , Error'
    , Info
    , Warn
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LogLevel = LogLevel' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern Debug :: LogLevel
pattern Debug = LogLevel' "DEBUG"

pattern Disabled :: LogLevel
pattern Disabled = LogLevel' "DISABLED"

pattern Error' :: LogLevel
pattern Error' = LogLevel' "ERROR"

pattern Info :: LogLevel
pattern Info = LogLevel' "INFO"

pattern Warn :: LogLevel
pattern Warn = LogLevel' "WARN"

{-# COMPLETE
  Debug,
  Disabled,
  Error',
  Info,
  Warn,
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
        0 -> Debug
        1 -> Disabled
        2 -> Error'
        3 -> Info
        4 -> Warn
        _ -> (error . showText) $ "Unknown index for LogLevel: " <> toText i
    fromEnum x = case x of
        Debug -> 0
        Disabled -> 1
        Error' -> 2
        Info -> 3
        Warn -> 4
        LogLevel' name -> (error . showText) $ "Unknown LogLevel: " <> original name

-- | Represents the bounds of /known/ $LogLevel.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LogLevel where
    minBound = Debug
    maxBound = Warn

instance Hashable     LogLevel
instance NFData       LogLevel
instance ToByteString LogLevel
instance ToQuery      LogLevel
instance ToHeader     LogLevel

instance ToJSON LogLevel where
    toJSON = toJSONText

instance FromJSON LogLevel where
    parseJSON = parseJSONText "LogLevel"
