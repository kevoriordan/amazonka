{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.OrientationCorrection
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.OrientationCorrection (
  OrientationCorrection (
    ..
    , Rotate0
    , Rotate180
    , Rotate270
    , Rotate90
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data OrientationCorrection = OrientationCorrection' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern Rotate0 :: OrientationCorrection
pattern Rotate0 = OrientationCorrection' "ROTATE_0"

pattern Rotate180 :: OrientationCorrection
pattern Rotate180 = OrientationCorrection' "ROTATE_180"

pattern Rotate270 :: OrientationCorrection
pattern Rotate270 = OrientationCorrection' "ROTATE_270"

pattern Rotate90 :: OrientationCorrection
pattern Rotate90 = OrientationCorrection' "ROTATE_90"

{-# COMPLETE
  Rotate0,
  Rotate180,
  Rotate270,
  Rotate90,
  OrientationCorrection' #-}

instance FromText OrientationCorrection where
    parser = (OrientationCorrection' . mk) <$> takeText

instance ToText OrientationCorrection where
    toText (OrientationCorrection' ci) = original ci

-- | Represents an enum of /known/ $OrientationCorrection.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OrientationCorrection where
    toEnum i = case i of
        0 -> Rotate0
        1 -> Rotate180
        2 -> Rotate270
        3 -> Rotate90
        _ -> (error . showText) $ "Unknown index for OrientationCorrection: " <> toText i
    fromEnum x = case x of
        Rotate0 -> 0
        Rotate180 -> 1
        Rotate270 -> 2
        Rotate90 -> 3
        OrientationCorrection' name -> (error . showText) $ "Unknown OrientationCorrection: " <> original name

-- | Represents the bounds of /known/ $OrientationCorrection.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OrientationCorrection where
    minBound = Rotate0
    maxBound = Rotate90

instance Hashable     OrientationCorrection
instance NFData       OrientationCorrection
instance ToByteString OrientationCorrection
instance ToQuery      OrientationCorrection
instance ToHeader     OrientationCorrection

instance FromJSON OrientationCorrection where
    parseJSON = parseJSONText "OrientationCorrection"
