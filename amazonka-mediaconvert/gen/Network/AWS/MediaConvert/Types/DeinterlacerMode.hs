{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DeinterlacerMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.DeinterlacerMode (
  DeinterlacerMode (
    ..
    , DMAdaptive
    , DMDeinterlace
    , DMInverseTelecine
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Use Deinterlacer (DeinterlaceMode) to choose how the service will do deinterlacing. Default is Deinterlace. - Deinterlace converts interlaced to progressive. - Inverse telecine converts Hard Telecine 29.97i to progressive 23.976p. - Adaptive auto-detects and converts to progressive.
data DeinterlacerMode = DeinterlacerMode' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern DMAdaptive :: DeinterlacerMode
pattern DMAdaptive = DeinterlacerMode' "ADAPTIVE"

pattern DMDeinterlace :: DeinterlacerMode
pattern DMDeinterlace = DeinterlacerMode' "DEINTERLACE"

pattern DMInverseTelecine :: DeinterlacerMode
pattern DMInverseTelecine = DeinterlacerMode' "INVERSE_TELECINE"

{-# COMPLETE
  DMAdaptive,
  DMDeinterlace,
  DMInverseTelecine,
  DeinterlacerMode' #-}

instance FromText DeinterlacerMode where
    parser = (DeinterlacerMode' . mk) <$> takeText

instance ToText DeinterlacerMode where
    toText (DeinterlacerMode' ci) = original ci

-- | Represents an enum of /known/ $DeinterlacerMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DeinterlacerMode where
    toEnum i = case i of
        0 -> DMAdaptive
        1 -> DMDeinterlace
        2 -> DMInverseTelecine
        _ -> (error . showText) $ "Unknown index for DeinterlacerMode: " <> toText i
    fromEnum x = case x of
        DMAdaptive -> 0
        DMDeinterlace -> 1
        DMInverseTelecine -> 2
        DeinterlacerMode' name -> (error . showText) $ "Unknown DeinterlacerMode: " <> original name

-- | Represents the bounds of /known/ $DeinterlacerMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DeinterlacerMode where
    minBound = DMAdaptive
    maxBound = DMInverseTelecine

instance Hashable     DeinterlacerMode
instance NFData       DeinterlacerMode
instance ToByteString DeinterlacerMode
instance ToQuery      DeinterlacerMode
instance ToHeader     DeinterlacerMode

instance ToJSON DeinterlacerMode where
    toJSON = toJSONText

instance FromJSON DeinterlacerMode where
    parseJSON = parseJSONText "DeinterlacerMode"
