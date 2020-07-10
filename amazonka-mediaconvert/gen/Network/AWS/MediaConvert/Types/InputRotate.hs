{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.InputRotate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.InputRotate (
  InputRotate (
    ..
    , Auto
    , Degree0
    , Degrees180
    , Degrees270
    , Degrees90
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Use Rotate (InputRotate) to specify how the service rotates your video. You can choose automatic rotation or specify a rotation. You can specify a clockwise rotation of 0, 90, 180, or 270 degrees. If your input video container is .mov or .mp4 and your input has rotation metadata, you can choose Automatic to have the service rotate your video according to the rotation specified in the metadata. The rotation must be within one degree of 90, 180, or 270 degrees. If the rotation metadata specifies any other rotation, the service will default to no rotation. By default, the service does no rotation, even if your input video has rotation metadata. The service doesn't pass through rotation metadata.
data InputRotate = InputRotate' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Auto :: InputRotate
pattern Auto = InputRotate' "AUTO"

pattern Degree0 :: InputRotate
pattern Degree0 = InputRotate' "DEGREE_0"

pattern Degrees180 :: InputRotate
pattern Degrees180 = InputRotate' "DEGREES_180"

pattern Degrees270 :: InputRotate
pattern Degrees270 = InputRotate' "DEGREES_270"

pattern Degrees90 :: InputRotate
pattern Degrees90 = InputRotate' "DEGREES_90"

{-# COMPLETE
  Auto,
  Degree0,
  Degrees180,
  Degrees270,
  Degrees90,
  InputRotate' #-}

instance FromText InputRotate where
    parser = (InputRotate' . mk) <$> takeText

instance ToText InputRotate where
    toText (InputRotate' ci) = original ci

-- | Represents an enum of /known/ $InputRotate.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InputRotate where
    toEnum i = case i of
        0 -> Auto
        1 -> Degree0
        2 -> Degrees180
        3 -> Degrees270
        4 -> Degrees90
        _ -> (error . showText) $ "Unknown index for InputRotate: " <> toText i
    fromEnum x = case x of
        Auto -> 0
        Degree0 -> 1
        Degrees180 -> 2
        Degrees270 -> 3
        Degrees90 -> 4
        InputRotate' name -> (error . showText) $ "Unknown InputRotate: " <> original name

-- | Represents the bounds of /known/ $InputRotate.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InputRotate where
    minBound = Auto
    maxBound = Degrees90

instance Hashable     InputRotate
instance NFData       InputRotate
instance ToByteString InputRotate
instance ToQuery      InputRotate
instance ToHeader     InputRotate

instance ToJSON InputRotate where
    toJSON = toJSONText

instance FromJSON InputRotate where
    parseJSON = parseJSONText "InputRotate"
