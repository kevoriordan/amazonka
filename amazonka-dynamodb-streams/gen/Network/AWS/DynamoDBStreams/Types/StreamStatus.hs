{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types.StreamStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDBStreams.Types.StreamStatus (
  StreamStatus (
    ..
    , Disabled
    , Disabling
    , Enabled
    , Enabling
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StreamStatus = StreamStatus' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Disabled :: StreamStatus
pattern Disabled = StreamStatus' "DISABLED"

pattern Disabling :: StreamStatus
pattern Disabling = StreamStatus' "DISABLING"

pattern Enabled :: StreamStatus
pattern Enabled = StreamStatus' "ENABLED"

pattern Enabling :: StreamStatus
pattern Enabling = StreamStatus' "ENABLING"

{-# COMPLETE
  Disabled,
  Disabling,
  Enabled,
  Enabling,
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
        0 -> Disabled
        1 -> Disabling
        2 -> Enabled
        3 -> Enabling
        _ -> (error . showText) $ "Unknown index for StreamStatus: " <> toText i
    fromEnum x = case x of
        Disabled -> 0
        Disabling -> 1
        Enabled -> 2
        Enabling -> 3
        StreamStatus' name -> (error . showText) $ "Unknown StreamStatus: " <> original name

-- | Represents the bounds of /known/ $StreamStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StreamStatus where
    minBound = Disabled
    maxBound = Enabling

instance Hashable     StreamStatus
instance NFData       StreamStatus
instance ToByteString StreamStatus
instance ToQuery      StreamStatus
instance ToHeader     StreamStatus

instance FromJSON StreamStatus where
    parseJSON = parseJSONText "StreamStatus"
