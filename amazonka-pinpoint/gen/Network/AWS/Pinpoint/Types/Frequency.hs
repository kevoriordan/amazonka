{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Frequency
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.Frequency (
  Frequency (
    ..
    , Daily
    , Event
    , Hourly
    , Monthly
    , Once
    , Weekly
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Frequency = Frequency' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern Daily :: Frequency
pattern Daily = Frequency' "DAILY"

pattern Event :: Frequency
pattern Event = Frequency' "EVENT"

pattern Hourly :: Frequency
pattern Hourly = Frequency' "HOURLY"

pattern Monthly :: Frequency
pattern Monthly = Frequency' "MONTHLY"

pattern Once :: Frequency
pattern Once = Frequency' "ONCE"

pattern Weekly :: Frequency
pattern Weekly = Frequency' "WEEKLY"

{-# COMPLETE
  Daily,
  Event,
  Hourly,
  Monthly,
  Once,
  Weekly,
  Frequency' #-}

instance FromText Frequency where
    parser = (Frequency' . mk) <$> takeText

instance ToText Frequency where
    toText (Frequency' ci) = original ci

-- | Represents an enum of /known/ $Frequency.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Frequency where
    toEnum i = case i of
        0 -> Daily
        1 -> Event
        2 -> Hourly
        3 -> Monthly
        4 -> Once
        5 -> Weekly
        _ -> (error . showText) $ "Unknown index for Frequency: " <> toText i
    fromEnum x = case x of
        Daily -> 0
        Event -> 1
        Hourly -> 2
        Monthly -> 3
        Once -> 4
        Weekly -> 5
        Frequency' name -> (error . showText) $ "Unknown Frequency: " <> original name

-- | Represents the bounds of /known/ $Frequency.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Frequency where
    minBound = Daily
    maxBound = Weekly

instance Hashable     Frequency
instance NFData       Frequency
instance ToByteString Frequency
instance ToQuery      Frequency
instance ToHeader     Frequency

instance ToJSON Frequency where
    toJSON = toJSONText

instance FromJSON Frequency where
    parseJSON = parseJSONText "Frequency"
