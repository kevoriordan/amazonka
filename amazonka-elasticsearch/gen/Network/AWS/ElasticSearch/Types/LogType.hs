{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.LogType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.LogType (
  LogType (
    ..
    , EsApplicationLogs
    , IndexSlowLogs
    , SearchSlowLogs
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Type of Log File, it can be one of the following:     * INDEX_SLOW_LOGS: Index slow logs contain insert requests that took more time than configured index query log threshold to execute.    * SEARCH_SLOW_LOGS: Search slow logs contain search queries that took more time than configured search query log threshold to execute.    * ES_APPLICATION_LOGS: Elasticsearch application logs contain information about errors and warnings raised during the operation of the service and can be useful for troubleshooting.
--
--
--
--
data LogType = LogType' (CI Text)
                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                           Generic)

pattern EsApplicationLogs :: LogType
pattern EsApplicationLogs = LogType' "ES_APPLICATION_LOGS"

pattern IndexSlowLogs :: LogType
pattern IndexSlowLogs = LogType' "INDEX_SLOW_LOGS"

pattern SearchSlowLogs :: LogType
pattern SearchSlowLogs = LogType' "SEARCH_SLOW_LOGS"

{-# COMPLETE
  EsApplicationLogs,
  IndexSlowLogs,
  SearchSlowLogs,
  LogType' #-}

instance FromText LogType where
    parser = (LogType' . mk) <$> takeText

instance ToText LogType where
    toText (LogType' ci) = original ci

-- | Represents an enum of /known/ $LogType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LogType where
    toEnum i = case i of
        0 -> EsApplicationLogs
        1 -> IndexSlowLogs
        2 -> SearchSlowLogs
        _ -> (error . showText) $ "Unknown index for LogType: " <> toText i
    fromEnum x = case x of
        EsApplicationLogs -> 0
        IndexSlowLogs -> 1
        SearchSlowLogs -> 2
        LogType' name -> (error . showText) $ "Unknown LogType: " <> original name

-- | Represents the bounds of /known/ $LogType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LogType where
    minBound = EsApplicationLogs
    maxBound = SearchSlowLogs

instance Hashable     LogType
instance NFData       LogType
instance ToByteString LogType
instance ToQuery      LogType
instance ToHeader     LogType

instance ToJSON LogType where
    toJSON = toJSONText

instance FromJSON LogType where
    parseJSON = parseJSONText "LogType"
