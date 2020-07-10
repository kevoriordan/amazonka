{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EventSeverity
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.EventSeverity (
  EventSeverity (
    ..
    , LevelDebug
    , LevelError'
    , LevelFatal
    , LevelInfo
    , LevelTrace
    , LevelWarn
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EventSeverity = EventSeverity' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern LevelDebug :: EventSeverity
pattern LevelDebug = EventSeverity' "DEBUG"

pattern LevelError' :: EventSeverity
pattern LevelError' = EventSeverity' "ERROR"

pattern LevelFatal :: EventSeverity
pattern LevelFatal = EventSeverity' "FATAL"

pattern LevelInfo :: EventSeverity
pattern LevelInfo = EventSeverity' "INFO"

pattern LevelTrace :: EventSeverity
pattern LevelTrace = EventSeverity' "TRACE"

pattern LevelWarn :: EventSeverity
pattern LevelWarn = EventSeverity' "WARN"

{-# COMPLETE
  LevelDebug,
  LevelError',
  LevelFatal,
  LevelInfo,
  LevelTrace,
  LevelWarn,
  EventSeverity' #-}

instance FromText EventSeverity where
    parser = (EventSeverity' . mk) <$> takeText

instance ToText EventSeverity where
    toText (EventSeverity' ci) = original ci

-- | Represents an enum of /known/ $EventSeverity.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EventSeverity where
    toEnum i = case i of
        0 -> LevelDebug
        1 -> LevelError'
        2 -> LevelFatal
        3 -> LevelInfo
        4 -> LevelTrace
        5 -> LevelWarn
        _ -> (error . showText) $ "Unknown index for EventSeverity: " <> toText i
    fromEnum x = case x of
        LevelDebug -> 0
        LevelError' -> 1
        LevelFatal -> 2
        LevelInfo -> 3
        LevelTrace -> 4
        LevelWarn -> 5
        EventSeverity' name -> (error . showText) $ "Unknown EventSeverity: " <> original name

-- | Represents the bounds of /known/ $EventSeverity.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EventSeverity where
    minBound = LevelDebug
    maxBound = LevelWarn

instance Hashable     EventSeverity
instance NFData       EventSeverity
instance ToByteString EventSeverity
instance ToQuery      EventSeverity
instance ToHeader     EventSeverity

instance FromXML EventSeverity where
    parseXML = parseXMLText "EventSeverity"
