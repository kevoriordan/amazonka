{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2IntraDcPrecision
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Mpeg2IntraDcPrecision (
  Mpeg2IntraDcPrecision (
    ..
    , MIDPAuto
    , MIDPIntraDcPrecision10
    , MIDPIntraDcPrecision11
    , MIDPIntraDcPrecision8
    , MIDPIntraDcPrecision9
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Use Intra DC precision (Mpeg2IntraDcPrecision) to set quantization precision for intra-block DC coefficients. If you choose the value auto, the service will automatically select the precision based on the per-frame compression ratio.
data Mpeg2IntraDcPrecision = Mpeg2IntraDcPrecision' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern MIDPAuto :: Mpeg2IntraDcPrecision
pattern MIDPAuto = Mpeg2IntraDcPrecision' "AUTO"

pattern MIDPIntraDcPrecision10 :: Mpeg2IntraDcPrecision
pattern MIDPIntraDcPrecision10 = Mpeg2IntraDcPrecision' "INTRA_DC_PRECISION_10"

pattern MIDPIntraDcPrecision11 :: Mpeg2IntraDcPrecision
pattern MIDPIntraDcPrecision11 = Mpeg2IntraDcPrecision' "INTRA_DC_PRECISION_11"

pattern MIDPIntraDcPrecision8 :: Mpeg2IntraDcPrecision
pattern MIDPIntraDcPrecision8 = Mpeg2IntraDcPrecision' "INTRA_DC_PRECISION_8"

pattern MIDPIntraDcPrecision9 :: Mpeg2IntraDcPrecision
pattern MIDPIntraDcPrecision9 = Mpeg2IntraDcPrecision' "INTRA_DC_PRECISION_9"

{-# COMPLETE
  MIDPAuto,
  MIDPIntraDcPrecision10,
  MIDPIntraDcPrecision11,
  MIDPIntraDcPrecision8,
  MIDPIntraDcPrecision9,
  Mpeg2IntraDcPrecision' #-}

instance FromText Mpeg2IntraDcPrecision where
    parser = (Mpeg2IntraDcPrecision' . mk) <$> takeText

instance ToText Mpeg2IntraDcPrecision where
    toText (Mpeg2IntraDcPrecision' ci) = original ci

-- | Represents an enum of /known/ $Mpeg2IntraDcPrecision.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Mpeg2IntraDcPrecision where
    toEnum i = case i of
        0 -> MIDPAuto
        1 -> MIDPIntraDcPrecision10
        2 -> MIDPIntraDcPrecision11
        3 -> MIDPIntraDcPrecision8
        4 -> MIDPIntraDcPrecision9
        _ -> (error . showText) $ "Unknown index for Mpeg2IntraDcPrecision: " <> toText i
    fromEnum x = case x of
        MIDPAuto -> 0
        MIDPIntraDcPrecision10 -> 1
        MIDPIntraDcPrecision11 -> 2
        MIDPIntraDcPrecision8 -> 3
        MIDPIntraDcPrecision9 -> 4
        Mpeg2IntraDcPrecision' name -> (error . showText) $ "Unknown Mpeg2IntraDcPrecision: " <> original name

-- | Represents the bounds of /known/ $Mpeg2IntraDcPrecision.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Mpeg2IntraDcPrecision where
    minBound = MIDPAuto
    maxBound = MIDPIntraDcPrecision9

instance Hashable     Mpeg2IntraDcPrecision
instance NFData       Mpeg2IntraDcPrecision
instance ToByteString Mpeg2IntraDcPrecision
instance ToQuery      Mpeg2IntraDcPrecision
instance ToHeader     Mpeg2IntraDcPrecision

instance ToJSON Mpeg2IntraDcPrecision where
    toJSON = toJSONText

instance FromJSON Mpeg2IntraDcPrecision where
    parseJSON = parseJSONText "Mpeg2IntraDcPrecision"
