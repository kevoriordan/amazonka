{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35NoRegionalBlackoutFlag
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Scte35NoRegionalBlackoutFlag (
  Scte35NoRegionalBlackoutFlag (
    ..
    , NoRegionalBlackout
    , RegionalBlackout
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Corresponds to the no_regional_blackout_flag parameter. A value of REGIONAL_BLACKOUT corresponds to 0 (false) in the SCTE-35 specification. If you include one of the "restriction" flags then you must include all four of them.
data Scte35NoRegionalBlackoutFlag = Scte35NoRegionalBlackoutFlag' (CI
                                                                     Text)
                                      deriving (Eq, Ord, Read, Show, Data,
                                                Typeable, Generic)

pattern NoRegionalBlackout :: Scte35NoRegionalBlackoutFlag
pattern NoRegionalBlackout = Scte35NoRegionalBlackoutFlag' "NO_REGIONAL_BLACKOUT"

pattern RegionalBlackout :: Scte35NoRegionalBlackoutFlag
pattern RegionalBlackout = Scte35NoRegionalBlackoutFlag' "REGIONAL_BLACKOUT"

{-# COMPLETE
  NoRegionalBlackout,
  RegionalBlackout,
  Scte35NoRegionalBlackoutFlag' #-}

instance FromText Scte35NoRegionalBlackoutFlag where
    parser = (Scte35NoRegionalBlackoutFlag' . mk) <$> takeText

instance ToText Scte35NoRegionalBlackoutFlag where
    toText (Scte35NoRegionalBlackoutFlag' ci) = original ci

-- | Represents an enum of /known/ $Scte35NoRegionalBlackoutFlag.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Scte35NoRegionalBlackoutFlag where
    toEnum i = case i of
        0 -> NoRegionalBlackout
        1 -> RegionalBlackout
        _ -> (error . showText) $ "Unknown index for Scte35NoRegionalBlackoutFlag: " <> toText i
    fromEnum x = case x of
        NoRegionalBlackout -> 0
        RegionalBlackout -> 1
        Scte35NoRegionalBlackoutFlag' name -> (error . showText) $ "Unknown Scte35NoRegionalBlackoutFlag: " <> original name

-- | Represents the bounds of /known/ $Scte35NoRegionalBlackoutFlag.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Scte35NoRegionalBlackoutFlag where
    minBound = NoRegionalBlackout
    maxBound = RegionalBlackout

instance Hashable     Scte35NoRegionalBlackoutFlag
instance NFData       Scte35NoRegionalBlackoutFlag
instance ToByteString Scte35NoRegionalBlackoutFlag
instance ToQuery      Scte35NoRegionalBlackoutFlag
instance ToHeader     Scte35NoRegionalBlackoutFlag

instance ToJSON Scte35NoRegionalBlackoutFlag where
    toJSON = toJSONText

instance FromJSON Scte35NoRegionalBlackoutFlag where
    parseJSON = parseJSONText "Scte35NoRegionalBlackoutFlag"
