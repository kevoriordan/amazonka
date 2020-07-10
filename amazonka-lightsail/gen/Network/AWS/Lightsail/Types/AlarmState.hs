{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.AlarmState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.AlarmState (
  AlarmState (
    ..
    , Alarm
    , InsufficientData
    , OK
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AlarmState = AlarmState' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern Alarm :: AlarmState
pattern Alarm = AlarmState' "ALARM"

pattern InsufficientData :: AlarmState
pattern InsufficientData = AlarmState' "INSUFFICIENT_DATA"

pattern OK :: AlarmState
pattern OK = AlarmState' "OK"

{-# COMPLETE
  Alarm,
  InsufficientData,
  OK,
  AlarmState' #-}

instance FromText AlarmState where
    parser = (AlarmState' . mk) <$> takeText

instance ToText AlarmState where
    toText (AlarmState' ci) = original ci

-- | Represents an enum of /known/ $AlarmState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AlarmState where
    toEnum i = case i of
        0 -> Alarm
        1 -> InsufficientData
        2 -> OK
        _ -> (error . showText) $ "Unknown index for AlarmState: " <> toText i
    fromEnum x = case x of
        Alarm -> 0
        InsufficientData -> 1
        OK -> 2
        AlarmState' name -> (error . showText) $ "Unknown AlarmState: " <> original name

-- | Represents the bounds of /known/ $AlarmState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AlarmState where
    minBound = Alarm
    maxBound = OK

instance Hashable     AlarmState
instance NFData       AlarmState
instance ToByteString AlarmState
instance ToQuery      AlarmState
instance ToHeader     AlarmState

instance ToJSON AlarmState where
    toJSON = toJSONText

instance FromJSON AlarmState where
    parseJSON = parseJSONText "AlarmState"
