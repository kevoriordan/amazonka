{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ProresTelecine
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.ProresTelecine (
  ProresTelecine (
    ..
    , PTHard
    , PTNone
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Only use Telecine (ProresTelecine) when you set Framerate (Framerate) to 29.970. Set Telecine (ProresTelecine) to Hard (hard) to produce a 29.97i output from a 23.976 input. Set it to Soft (soft) to produce 23.976 output and leave converstion to the player.
data ProresTelecine = ProresTelecine' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern PTHard :: ProresTelecine
pattern PTHard = ProresTelecine' "HARD"

pattern PTNone :: ProresTelecine
pattern PTNone = ProresTelecine' "NONE"

{-# COMPLETE
  PTHard,
  PTNone,
  ProresTelecine' #-}

instance FromText ProresTelecine where
    parser = (ProresTelecine' . mk) <$> takeText

instance ToText ProresTelecine where
    toText (ProresTelecine' ci) = original ci

-- | Represents an enum of /known/ $ProresTelecine.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProresTelecine where
    toEnum i = case i of
        0 -> PTHard
        1 -> PTNone
        _ -> (error . showText) $ "Unknown index for ProresTelecine: " <> toText i
    fromEnum x = case x of
        PTHard -> 0
        PTNone -> 1
        ProresTelecine' name -> (error . showText) $ "Unknown ProresTelecine: " <> original name

-- | Represents the bounds of /known/ $ProresTelecine.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProresTelecine where
    minBound = PTHard
    maxBound = PTNone

instance Hashable     ProresTelecine
instance NFData       ProresTelecine
instance ToByteString ProresTelecine
instance ToQuery      ProresTelecine
instance ToHeader     ProresTelecine

instance ToJSON ProresTelecine where
    toJSON = toJSONText

instance FromJSON ProresTelecine where
    parseJSON = parseJSONText "ProresTelecine"
