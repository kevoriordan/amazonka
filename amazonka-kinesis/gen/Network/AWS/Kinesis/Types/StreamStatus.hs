{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.StreamStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.StreamStatus (
  StreamStatus (
    ..
    , SSActive
    , SSCreating
    , SSDeleting
    , SSUpdating
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StreamStatus = StreamStatus' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern SSActive :: StreamStatus
pattern SSActive = StreamStatus' "ACTIVE"

pattern SSCreating :: StreamStatus
pattern SSCreating = StreamStatus' "CREATING"

pattern SSDeleting :: StreamStatus
pattern SSDeleting = StreamStatus' "DELETING"

pattern SSUpdating :: StreamStatus
pattern SSUpdating = StreamStatus' "UPDATING"

{-# COMPLETE
  SSActive,
  SSCreating,
  SSDeleting,
  SSUpdating,
  StreamStatus' #-}

instance FromText StreamStatus where
    parser = (StreamStatus' . mk) <$> takeText

instance ToText StreamStatus where
    toText (StreamStatus' ci) = original ci

-- | Represents an enum of /known/ $StreamStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StreamStatus where
    toEnum i = case i of
        0 -> SSActive
        1 -> SSCreating
        2 -> SSDeleting
        3 -> SSUpdating
        _ -> (error . showText) $ "Unknown index for StreamStatus: " <> toText i
    fromEnum x = case x of
        SSActive -> 0
        SSCreating -> 1
        SSDeleting -> 2
        SSUpdating -> 3
        StreamStatus' name -> (error . showText) $ "Unknown StreamStatus: " <> original name

-- | Represents the bounds of /known/ $StreamStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StreamStatus where
    minBound = SSActive
    maxBound = SSUpdating

instance Hashable     StreamStatus
instance NFData       StreamStatus
instance ToByteString StreamStatus
instance ToQuery      StreamStatus
instance ToHeader     StreamStatus

instance FromJSON StreamStatus where
    parseJSON = parseJSONText "StreamStatus"
