{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ActivityStreamStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.ActivityStreamStatus (
  ActivityStreamStatus (
    ..
    , Started
    , Starting
    , Stopped
    , Stopping
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ActivityStreamStatus = ActivityStreamStatus' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern Started :: ActivityStreamStatus
pattern Started = ActivityStreamStatus' "started"

pattern Starting :: ActivityStreamStatus
pattern Starting = ActivityStreamStatus' "starting"

pattern Stopped :: ActivityStreamStatus
pattern Stopped = ActivityStreamStatus' "stopped"

pattern Stopping :: ActivityStreamStatus
pattern Stopping = ActivityStreamStatus' "stopping"

{-# COMPLETE
  Started,
  Starting,
  Stopped,
  Stopping,
  ActivityStreamStatus' #-}

instance FromText ActivityStreamStatus where
    parser = (ActivityStreamStatus' . mk) <$> takeText

instance ToText ActivityStreamStatus where
    toText (ActivityStreamStatus' ci) = original ci

-- | Represents an enum of /known/ $ActivityStreamStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ActivityStreamStatus where
    toEnum i = case i of
        0 -> Started
        1 -> Starting
        2 -> Stopped
        3 -> Stopping
        _ -> (error . showText) $ "Unknown index for ActivityStreamStatus: " <> toText i
    fromEnum x = case x of
        Started -> 0
        Starting -> 1
        Stopped -> 2
        Stopping -> 3
        ActivityStreamStatus' name -> (error . showText) $ "Unknown ActivityStreamStatus: " <> original name

-- | Represents the bounds of /known/ $ActivityStreamStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ActivityStreamStatus where
    minBound = Started
    maxBound = Stopping

instance Hashable     ActivityStreamStatus
instance NFData       ActivityStreamStatus
instance ToByteString ActivityStreamStatus
instance ToQuery      ActivityStreamStatus
instance ToHeader     ActivityStreamStatus

instance FromXML ActivityStreamStatus where
    parseXML = parseXMLText "ActivityStreamStatus"
