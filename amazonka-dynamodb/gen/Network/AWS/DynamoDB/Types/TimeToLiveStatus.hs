{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.TimeToLiveStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.TimeToLiveStatus (
  TimeToLiveStatus (
    ..
    , Disabled
    , Disabling
    , Enabled
    , Enabling
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TimeToLiveStatus = TimeToLiveStatus' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern Disabled :: TimeToLiveStatus
pattern Disabled = TimeToLiveStatus' "DISABLED"

pattern Disabling :: TimeToLiveStatus
pattern Disabling = TimeToLiveStatus' "DISABLING"

pattern Enabled :: TimeToLiveStatus
pattern Enabled = TimeToLiveStatus' "ENABLED"

pattern Enabling :: TimeToLiveStatus
pattern Enabling = TimeToLiveStatus' "ENABLING"

{-# COMPLETE
  Disabled,
  Disabling,
  Enabled,
  Enabling,
  TimeToLiveStatus' #-}

instance FromText TimeToLiveStatus where
    parser = (TimeToLiveStatus' . mk) <$> takeText

instance ToText TimeToLiveStatus where
    toText (TimeToLiveStatus' ci) = original ci

-- | Represents an enum of /known/ $TimeToLiveStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TimeToLiveStatus where
    toEnum i = case i of
        0 -> Disabled
        1 -> Disabling
        2 -> Enabled
        3 -> Enabling
        _ -> (error . showText) $ "Unknown index for TimeToLiveStatus: " <> toText i
    fromEnum x = case x of
        Disabled -> 0
        Disabling -> 1
        Enabled -> 2
        Enabling -> 3
        TimeToLiveStatus' name -> (error . showText) $ "Unknown TimeToLiveStatus: " <> original name

-- | Represents the bounds of /known/ $TimeToLiveStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TimeToLiveStatus where
    minBound = Disabled
    maxBound = Enabling

instance Hashable     TimeToLiveStatus
instance NFData       TimeToLiveStatus
instance ToByteString TimeToLiveStatus
instance ToQuery      TimeToLiveStatus
instance ToHeader     TimeToLiveStatus

instance FromJSON TimeToLiveStatus where
    parseJSON = parseJSONText "TimeToLiveStatus"
