{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ColorSpaceConversion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.ColorSpaceConversion (
  ColorSpaceConversion (
    ..
    , CSCForce601
    , CSCForce709
    , CSCForceHDR10
    , CSCForceHlg2020
    , CSCNone
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Specify the color space you want for this output. The service supports conversion between HDR formats, between SDR formats, from SDR to HDR, and from HDR to SDR. SDR to HDR conversion doesn't upgrade the dynamic range. The converted video has an HDR format, but visually appears the same as an unconverted output. HDR to SDR conversion uses Elemental tone mapping technology to approximate the outcome of manually regrading from HDR to SDR.
data ColorSpaceConversion = ColorSpaceConversion' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern CSCForce601 :: ColorSpaceConversion
pattern CSCForce601 = ColorSpaceConversion' "FORCE_601"

pattern CSCForce709 :: ColorSpaceConversion
pattern CSCForce709 = ColorSpaceConversion' "FORCE_709"

pattern CSCForceHDR10 :: ColorSpaceConversion
pattern CSCForceHDR10 = ColorSpaceConversion' "FORCE_HDR10"

pattern CSCForceHlg2020 :: ColorSpaceConversion
pattern CSCForceHlg2020 = ColorSpaceConversion' "FORCE_HLG_2020"

pattern CSCNone :: ColorSpaceConversion
pattern CSCNone = ColorSpaceConversion' "NONE"

{-# COMPLETE
  CSCForce601,
  CSCForce709,
  CSCForceHDR10,
  CSCForceHlg2020,
  CSCNone,
  ColorSpaceConversion' #-}

instance FromText ColorSpaceConversion where
    parser = (ColorSpaceConversion' . mk) <$> takeText

instance ToText ColorSpaceConversion where
    toText (ColorSpaceConversion' ci) = original ci

-- | Represents an enum of /known/ $ColorSpaceConversion.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ColorSpaceConversion where
    toEnum i = case i of
        0 -> CSCForce601
        1 -> CSCForce709
        2 -> CSCForceHDR10
        3 -> CSCForceHlg2020
        4 -> CSCNone
        _ -> (error . showText) $ "Unknown index for ColorSpaceConversion: " <> toText i
    fromEnum x = case x of
        CSCForce601 -> 0
        CSCForce709 -> 1
        CSCForceHDR10 -> 2
        CSCForceHlg2020 -> 3
        CSCNone -> 4
        ColorSpaceConversion' name -> (error . showText) $ "Unknown ColorSpaceConversion: " <> original name

-- | Represents the bounds of /known/ $ColorSpaceConversion.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ColorSpaceConversion where
    minBound = CSCForce601
    maxBound = CSCNone

instance Hashable     ColorSpaceConversion
instance NFData       ColorSpaceConversion
instance ToByteString ColorSpaceConversion
instance ToQuery      ColorSpaceConversion
instance ToHeader     ColorSpaceConversion

instance ToJSON ColorSpaceConversion where
    toJSON = toJSONText

instance FromJSON ColorSpaceConversion where
    parseJSON = parseJSONText "ColorSpaceConversion"
