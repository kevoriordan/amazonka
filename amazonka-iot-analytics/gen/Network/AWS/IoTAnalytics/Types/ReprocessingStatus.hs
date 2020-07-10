{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ReprocessingStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.ReprocessingStatus (
  ReprocessingStatus (
    ..
    , Cancelled
    , Failed
    , Running
    , Succeeded
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ReprocessingStatus = ReprocessingStatus' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern Cancelled :: ReprocessingStatus
pattern Cancelled = ReprocessingStatus' "CANCELLED"

pattern Failed :: ReprocessingStatus
pattern Failed = ReprocessingStatus' "FAILED"

pattern Running :: ReprocessingStatus
pattern Running = ReprocessingStatus' "RUNNING"

pattern Succeeded :: ReprocessingStatus
pattern Succeeded = ReprocessingStatus' "SUCCEEDED"

{-# COMPLETE
  Cancelled,
  Failed,
  Running,
  Succeeded,
  ReprocessingStatus' #-}

instance FromText ReprocessingStatus where
    parser = (ReprocessingStatus' . mk) <$> takeText

instance ToText ReprocessingStatus where
    toText (ReprocessingStatus' ci) = original ci

-- | Represents an enum of /known/ $ReprocessingStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReprocessingStatus where
    toEnum i = case i of
        0 -> Cancelled
        1 -> Failed
        2 -> Running
        3 -> Succeeded
        _ -> (error . showText) $ "Unknown index for ReprocessingStatus: " <> toText i
    fromEnum x = case x of
        Cancelled -> 0
        Failed -> 1
        Running -> 2
        Succeeded -> 3
        ReprocessingStatus' name -> (error . showText) $ "Unknown ReprocessingStatus: " <> original name

-- | Represents the bounds of /known/ $ReprocessingStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReprocessingStatus where
    minBound = Cancelled
    maxBound = Succeeded

instance Hashable     ReprocessingStatus
instance NFData       ReprocessingStatus
instance ToByteString ReprocessingStatus
instance ToQuery      ReprocessingStatus
instance ToHeader     ReprocessingStatus

instance FromJSON ReprocessingStatus where
    parseJSON = parseJSONText "ReprocessingStatus"
