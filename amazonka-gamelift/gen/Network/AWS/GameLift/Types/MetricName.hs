{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.MetricName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.MetricName (
  MetricName (
    ..
    , ActivatingGameSessions
    , ActiveGameSessions
    , ActiveInstances
    , AvailableGameSessions
    , AvailablePlayerSessions
    , CurrentPlayerSessions
    , IdleInstances
    , PercentAvailableGameSessions
    , PercentIdleInstances
    , QueueDepth
    , WaitTime
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MetricName = MetricName' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern ActivatingGameSessions :: MetricName
pattern ActivatingGameSessions = MetricName' "ActivatingGameSessions"

pattern ActiveGameSessions :: MetricName
pattern ActiveGameSessions = MetricName' "ActiveGameSessions"

pattern ActiveInstances :: MetricName
pattern ActiveInstances = MetricName' "ActiveInstances"

pattern AvailableGameSessions :: MetricName
pattern AvailableGameSessions = MetricName' "AvailableGameSessions"

pattern AvailablePlayerSessions :: MetricName
pattern AvailablePlayerSessions = MetricName' "AvailablePlayerSessions"

pattern CurrentPlayerSessions :: MetricName
pattern CurrentPlayerSessions = MetricName' "CurrentPlayerSessions"

pattern IdleInstances :: MetricName
pattern IdleInstances = MetricName' "IdleInstances"

pattern PercentAvailableGameSessions :: MetricName
pattern PercentAvailableGameSessions = MetricName' "PercentAvailableGameSessions"

pattern PercentIdleInstances :: MetricName
pattern PercentIdleInstances = MetricName' "PercentIdleInstances"

pattern QueueDepth :: MetricName
pattern QueueDepth = MetricName' "QueueDepth"

pattern WaitTime :: MetricName
pattern WaitTime = MetricName' "WaitTime"

{-# COMPLETE
  ActivatingGameSessions,
  ActiveGameSessions,
  ActiveInstances,
  AvailableGameSessions,
  AvailablePlayerSessions,
  CurrentPlayerSessions,
  IdleInstances,
  PercentAvailableGameSessions,
  PercentIdleInstances,
  QueueDepth,
  WaitTime,
  MetricName' #-}

instance FromText MetricName where
    parser = (MetricName' . mk) <$> takeText

instance ToText MetricName where
    toText (MetricName' ci) = original ci

-- | Represents an enum of /known/ $MetricName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MetricName where
    toEnum i = case i of
        0 -> ActivatingGameSessions
        1 -> ActiveGameSessions
        2 -> ActiveInstances
        3 -> AvailableGameSessions
        4 -> AvailablePlayerSessions
        5 -> CurrentPlayerSessions
        6 -> IdleInstances
        7 -> PercentAvailableGameSessions
        8 -> PercentIdleInstances
        9 -> QueueDepth
        10 -> WaitTime
        _ -> (error . showText) $ "Unknown index for MetricName: " <> toText i
    fromEnum x = case x of
        ActivatingGameSessions -> 0
        ActiveGameSessions -> 1
        ActiveInstances -> 2
        AvailableGameSessions -> 3
        AvailablePlayerSessions -> 4
        CurrentPlayerSessions -> 5
        IdleInstances -> 6
        PercentAvailableGameSessions -> 7
        PercentIdleInstances -> 8
        QueueDepth -> 9
        WaitTime -> 10
        MetricName' name -> (error . showText) $ "Unknown MetricName: " <> original name

-- | Represents the bounds of /known/ $MetricName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MetricName where
    minBound = ActivatingGameSessions
    maxBound = WaitTime

instance Hashable     MetricName
instance NFData       MetricName
instance ToByteString MetricName
instance ToQuery      MetricName
instance ToHeader     MetricName

instance ToJSON MetricName where
    toJSON = toJSONText

instance FromJSON MetricName where
    parseJSON = parseJSONText "MetricName"
