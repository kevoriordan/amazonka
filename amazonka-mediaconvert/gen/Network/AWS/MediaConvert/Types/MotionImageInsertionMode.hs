{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MotionImageInsertionMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.MotionImageInsertionMode (
  MotionImageInsertionMode (
    ..
    , Mov
    , Png
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Choose the type of motion graphic asset that you are providing for your overlay. You can choose either a .mov file or a series of .png files.
data MotionImageInsertionMode = MotionImageInsertionMode' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern Mov :: MotionImageInsertionMode
pattern Mov = MotionImageInsertionMode' "MOV"

pattern Png :: MotionImageInsertionMode
pattern Png = MotionImageInsertionMode' "PNG"

{-# COMPLETE
  Mov,
  Png,
  MotionImageInsertionMode' #-}

instance FromText MotionImageInsertionMode where
    parser = (MotionImageInsertionMode' . mk) <$> takeText

instance ToText MotionImageInsertionMode where
    toText (MotionImageInsertionMode' ci) = original ci

-- | Represents an enum of /known/ $MotionImageInsertionMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MotionImageInsertionMode where
    toEnum i = case i of
        0 -> Mov
        1 -> Png
        _ -> (error . showText) $ "Unknown index for MotionImageInsertionMode: " <> toText i
    fromEnum x = case x of
        Mov -> 0
        Png -> 1
        MotionImageInsertionMode' name -> (error . showText) $ "Unknown MotionImageInsertionMode: " <> original name

-- | Represents the bounds of /known/ $MotionImageInsertionMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MotionImageInsertionMode where
    minBound = Mov
    maxBound = Png

instance Hashable     MotionImageInsertionMode
instance NFData       MotionImageInsertionMode
instance ToByteString MotionImageInsertionMode
instance ToQuery      MotionImageInsertionMode
instance ToHeader     MotionImageInsertionMode

instance ToJSON MotionImageInsertionMode where
    toJSON = toJSONText

instance FromJSON MotionImageInsertionMode where
    parseJSON = parseJSONText "MotionImageInsertionMode"
