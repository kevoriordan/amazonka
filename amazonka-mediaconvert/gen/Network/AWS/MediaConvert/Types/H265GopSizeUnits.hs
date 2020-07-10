{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265GopSizeUnits
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H265GopSizeUnits (
  H265GopSizeUnits (
    ..
    , Frames
    , Seconds
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Indicates if the GOP Size in H265 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
data H265GopSizeUnits = H265GopSizeUnits' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern Frames :: H265GopSizeUnits
pattern Frames = H265GopSizeUnits' "FRAMES"

pattern Seconds :: H265GopSizeUnits
pattern Seconds = H265GopSizeUnits' "SECONDS"

{-# COMPLETE
  Frames,
  Seconds,
  H265GopSizeUnits' #-}

instance FromText H265GopSizeUnits where
    parser = (H265GopSizeUnits' . mk) <$> takeText

instance ToText H265GopSizeUnits where
    toText (H265GopSizeUnits' ci) = original ci

-- | Represents an enum of /known/ $H265GopSizeUnits.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H265GopSizeUnits where
    toEnum i = case i of
        0 -> Frames
        1 -> Seconds
        _ -> (error . showText) $ "Unknown index for H265GopSizeUnits: " <> toText i
    fromEnum x = case x of
        Frames -> 0
        Seconds -> 1
        H265GopSizeUnits' name -> (error . showText) $ "Unknown H265GopSizeUnits: " <> original name

-- | Represents the bounds of /known/ $H265GopSizeUnits.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H265GopSizeUnits where
    minBound = Frames
    maxBound = Seconds

instance Hashable     H265GopSizeUnits
instance NFData       H265GopSizeUnits
instance ToByteString H265GopSizeUnits
instance ToQuery      H265GopSizeUnits
instance ToHeader     H265GopSizeUnits

instance ToJSON H265GopSizeUnits where
    toJSON = toJSONText

instance FromJSON H265GopSizeUnits where
    parseJSON = parseJSONText "H265GopSizeUnits"
