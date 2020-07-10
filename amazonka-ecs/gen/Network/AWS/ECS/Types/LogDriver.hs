{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.LogDriver
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.LogDriver (
  LogDriver (
    ..
    , LDAWSfirelens
    , LDAWSlogs
    , LDFluentd
    , LDGelf
    , LDJSONFile
    , LDJournald
    , LDSplunk
    , LDSyslog
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LogDriver = LogDriver' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern LDAWSfirelens :: LogDriver
pattern LDAWSfirelens = LogDriver' "awsfirelens"

pattern LDAWSlogs :: LogDriver
pattern LDAWSlogs = LogDriver' "awslogs"

pattern LDFluentd :: LogDriver
pattern LDFluentd = LogDriver' "fluentd"

pattern LDGelf :: LogDriver
pattern LDGelf = LogDriver' "gelf"

pattern LDJSONFile :: LogDriver
pattern LDJSONFile = LogDriver' "json-file"

pattern LDJournald :: LogDriver
pattern LDJournald = LogDriver' "journald"

pattern LDSplunk :: LogDriver
pattern LDSplunk = LogDriver' "splunk"

pattern LDSyslog :: LogDriver
pattern LDSyslog = LogDriver' "syslog"

{-# COMPLETE
  LDAWSfirelens,
  LDAWSlogs,
  LDFluentd,
  LDGelf,
  LDJSONFile,
  LDJournald,
  LDSplunk,
  LDSyslog,
  LogDriver' #-}

instance FromText LogDriver where
    parser = (LogDriver' . mk) <$> takeText

instance ToText LogDriver where
    toText (LogDriver' ci) = original ci

-- | Represents an enum of /known/ $LogDriver.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LogDriver where
    toEnum i = case i of
        0 -> LDAWSfirelens
        1 -> LDAWSlogs
        2 -> LDFluentd
        3 -> LDGelf
        4 -> LDJSONFile
        5 -> LDJournald
        6 -> LDSplunk
        7 -> LDSyslog
        _ -> (error . showText) $ "Unknown index for LogDriver: " <> toText i
    fromEnum x = case x of
        LDAWSfirelens -> 0
        LDAWSlogs -> 1
        LDFluentd -> 2
        LDGelf -> 3
        LDJSONFile -> 4
        LDJournald -> 5
        LDSplunk -> 6
        LDSyslog -> 7
        LogDriver' name -> (error . showText) $ "Unknown LogDriver: " <> original name

-- | Represents the bounds of /known/ $LogDriver.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LogDriver where
    minBound = LDAWSfirelens
    maxBound = LDSyslog

instance Hashable     LogDriver
instance NFData       LogDriver
instance ToByteString LogDriver
instance ToQuery      LogDriver
instance ToHeader     LogDriver

instance ToJSON LogDriver where
    toJSON = toJSONText

instance FromJSON LogDriver where
    parseJSON = parseJSONText "LogDriver"
