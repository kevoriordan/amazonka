{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3PhaseControl
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Eac3PhaseControl (
  Eac3PhaseControl (
    ..
    , NoShift
    , Shift90Degrees
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Controls the amount of phase-shift applied to the surround channels. Only used for 3/2 coding mode.
data Eac3PhaseControl = Eac3PhaseControl' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern NoShift :: Eac3PhaseControl
pattern NoShift = Eac3PhaseControl' "NO_SHIFT"

pattern Shift90Degrees :: Eac3PhaseControl
pattern Shift90Degrees = Eac3PhaseControl' "SHIFT_90_DEGREES"

{-# COMPLETE
  NoShift,
  Shift90Degrees,
  Eac3PhaseControl' #-}

instance FromText Eac3PhaseControl where
    parser = (Eac3PhaseControl' . mk) <$> takeText

instance ToText Eac3PhaseControl where
    toText (Eac3PhaseControl' ci) = original ci

-- | Represents an enum of /known/ $Eac3PhaseControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Eac3PhaseControl where
    toEnum i = case i of
        0 -> NoShift
        1 -> Shift90Degrees
        _ -> (error . showText) $ "Unknown index for Eac3PhaseControl: " <> toText i
    fromEnum x = case x of
        NoShift -> 0
        Shift90Degrees -> 1
        Eac3PhaseControl' name -> (error . showText) $ "Unknown Eac3PhaseControl: " <> original name

-- | Represents the bounds of /known/ $Eac3PhaseControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Eac3PhaseControl where
    minBound = NoShift
    maxBound = Shift90Degrees

instance Hashable     Eac3PhaseControl
instance NFData       Eac3PhaseControl
instance ToByteString Eac3PhaseControl
instance ToQuery      Eac3PhaseControl
instance ToHeader     Eac3PhaseControl

instance ToJSON Eac3PhaseControl where
    toJSON = toJSONText

instance FromJSON Eac3PhaseControl where
    parseJSON = parseJSONText "Eac3PhaseControl"
