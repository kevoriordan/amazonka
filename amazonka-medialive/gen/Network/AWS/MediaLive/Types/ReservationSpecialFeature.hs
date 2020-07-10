{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ReservationSpecialFeature
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.ReservationSpecialFeature (
  ReservationSpecialFeature (
    ..
    , AdvancedAudio
    , AudioNormalization
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Special features, 'ADVANCED_AUDIO' or 'AUDIO_NORMALIZATION'
data ReservationSpecialFeature = ReservationSpecialFeature' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern AdvancedAudio :: ReservationSpecialFeature
pattern AdvancedAudio = ReservationSpecialFeature' "ADVANCED_AUDIO"

pattern AudioNormalization :: ReservationSpecialFeature
pattern AudioNormalization = ReservationSpecialFeature' "AUDIO_NORMALIZATION"

{-# COMPLETE
  AdvancedAudio,
  AudioNormalization,
  ReservationSpecialFeature' #-}

instance FromText ReservationSpecialFeature where
    parser = (ReservationSpecialFeature' . mk) <$> takeText

instance ToText ReservationSpecialFeature where
    toText (ReservationSpecialFeature' ci) = original ci

-- | Represents an enum of /known/ $ReservationSpecialFeature.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReservationSpecialFeature where
    toEnum i = case i of
        0 -> AdvancedAudio
        1 -> AudioNormalization
        _ -> (error . showText) $ "Unknown index for ReservationSpecialFeature: " <> toText i
    fromEnum x = case x of
        AdvancedAudio -> 0
        AudioNormalization -> 1
        ReservationSpecialFeature' name -> (error . showText) $ "Unknown ReservationSpecialFeature: " <> original name

-- | Represents the bounds of /known/ $ReservationSpecialFeature.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReservationSpecialFeature where
    minBound = AdvancedAudio
    maxBound = AudioNormalization

instance Hashable     ReservationSpecialFeature
instance NFData       ReservationSpecialFeature
instance ToByteString ReservationSpecialFeature
instance ToQuery      ReservationSpecialFeature
instance ToHeader     ReservationSpecialFeature

instance FromJSON ReservationSpecialFeature where
    parseJSON = parseJSONText "ReservationSpecialFeature"
