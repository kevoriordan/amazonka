{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3StereoDownmix
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Eac3StereoDownmix (
  Eac3StereoDownmix (
    ..
    , ESDDPL2
    , ESDLoRo
    , ESDLtRt
    , ESDNotIndicated
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Choose how the service does stereo downmixing. This setting only applies if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Stereo downmix (Eac3StereoDownmix).
data Eac3StereoDownmix = Eac3StereoDownmix' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern ESDDPL2 :: Eac3StereoDownmix
pattern ESDDPL2 = Eac3StereoDownmix' "DPL2"

pattern ESDLoRo :: Eac3StereoDownmix
pattern ESDLoRo = Eac3StereoDownmix' "LO_RO"

pattern ESDLtRt :: Eac3StereoDownmix
pattern ESDLtRt = Eac3StereoDownmix' "LT_RT"

pattern ESDNotIndicated :: Eac3StereoDownmix
pattern ESDNotIndicated = Eac3StereoDownmix' "NOT_INDICATED"

{-# COMPLETE
  ESDDPL2,
  ESDLoRo,
  ESDLtRt,
  ESDNotIndicated,
  Eac3StereoDownmix' #-}

instance FromText Eac3StereoDownmix where
    parser = (Eac3StereoDownmix' . mk) <$> takeText

instance ToText Eac3StereoDownmix where
    toText (Eac3StereoDownmix' ci) = original ci

-- | Represents an enum of /known/ $Eac3StereoDownmix.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Eac3StereoDownmix where
    toEnum i = case i of
        0 -> ESDDPL2
        1 -> ESDLoRo
        2 -> ESDLtRt
        3 -> ESDNotIndicated
        _ -> (error . showText) $ "Unknown index for Eac3StereoDownmix: " <> toText i
    fromEnum x = case x of
        ESDDPL2 -> 0
        ESDLoRo -> 1
        ESDLtRt -> 2
        ESDNotIndicated -> 3
        Eac3StereoDownmix' name -> (error . showText) $ "Unknown Eac3StereoDownmix: " <> original name

-- | Represents the bounds of /known/ $Eac3StereoDownmix.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Eac3StereoDownmix where
    minBound = ESDDPL2
    maxBound = ESDNotIndicated

instance Hashable     Eac3StereoDownmix
instance NFData       Eac3StereoDownmix
instance ToByteString Eac3StereoDownmix
instance ToQuery      Eac3StereoDownmix
instance ToHeader     Eac3StereoDownmix

instance ToJSON Eac3StereoDownmix where
    toJSON = toJSONText

instance FromJSON Eac3StereoDownmix where
    parseJSON = parseJSONText "Eac3StereoDownmix"
