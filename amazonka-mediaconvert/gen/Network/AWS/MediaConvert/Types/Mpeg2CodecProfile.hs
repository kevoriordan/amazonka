{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2CodecProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Mpeg2CodecProfile (
  Mpeg2CodecProfile (
    ..
    , Main
    , Profile422
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Use Profile (Mpeg2CodecProfile) to set the MPEG-2 profile for the video output.
data Mpeg2CodecProfile = Mpeg2CodecProfile' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern Main :: Mpeg2CodecProfile
pattern Main = Mpeg2CodecProfile' "MAIN"

pattern Profile422 :: Mpeg2CodecProfile
pattern Profile422 = Mpeg2CodecProfile' "PROFILE_422"

{-# COMPLETE
  Main,
  Profile422,
  Mpeg2CodecProfile' #-}

instance FromText Mpeg2CodecProfile where
    parser = (Mpeg2CodecProfile' . mk) <$> takeText

instance ToText Mpeg2CodecProfile where
    toText (Mpeg2CodecProfile' ci) = original ci

-- | Represents an enum of /known/ $Mpeg2CodecProfile.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Mpeg2CodecProfile where
    toEnum i = case i of
        0 -> Main
        1 -> Profile422
        _ -> (error . showText) $ "Unknown index for Mpeg2CodecProfile: " <> toText i
    fromEnum x = case x of
        Main -> 0
        Profile422 -> 1
        Mpeg2CodecProfile' name -> (error . showText) $ "Unknown Mpeg2CodecProfile: " <> original name

-- | Represents the bounds of /known/ $Mpeg2CodecProfile.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Mpeg2CodecProfile where
    minBound = Main
    maxBound = Profile422

instance Hashable     Mpeg2CodecProfile
instance NFData       Mpeg2CodecProfile
instance ToByteString Mpeg2CodecProfile
instance ToQuery      Mpeg2CodecProfile
instance ToHeader     Mpeg2CodecProfile

instance ToJSON Mpeg2CodecProfile where
    toJSON = toJSONText

instance FromJSON Mpeg2CodecProfile where
    parseJSON = parseJSONText "Mpeg2CodecProfile"
