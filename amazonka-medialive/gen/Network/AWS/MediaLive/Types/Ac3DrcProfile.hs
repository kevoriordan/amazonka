{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Ac3DrcProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Ac3DrcProfile (
  Ac3DrcProfile (
    ..
    , ADPFilmStandard
    , ADPNone
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Ac3 Drc Profile
data Ac3DrcProfile = Ac3DrcProfile' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern ADPFilmStandard :: Ac3DrcProfile
pattern ADPFilmStandard = Ac3DrcProfile' "FILM_STANDARD"

pattern ADPNone :: Ac3DrcProfile
pattern ADPNone = Ac3DrcProfile' "NONE"

{-# COMPLETE
  ADPFilmStandard,
  ADPNone,
  Ac3DrcProfile' #-}

instance FromText Ac3DrcProfile where
    parser = (Ac3DrcProfile' . mk) <$> takeText

instance ToText Ac3DrcProfile where
    toText (Ac3DrcProfile' ci) = original ci

-- | Represents an enum of /known/ $Ac3DrcProfile.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Ac3DrcProfile where
    toEnum i = case i of
        0 -> ADPFilmStandard
        1 -> ADPNone
        _ -> (error . showText) $ "Unknown index for Ac3DrcProfile: " <> toText i
    fromEnum x = case x of
        ADPFilmStandard -> 0
        ADPNone -> 1
        Ac3DrcProfile' name -> (error . showText) $ "Unknown Ac3DrcProfile: " <> original name

-- | Represents the bounds of /known/ $Ac3DrcProfile.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Ac3DrcProfile where
    minBound = ADPFilmStandard
    maxBound = ADPNone

instance Hashable     Ac3DrcProfile
instance NFData       Ac3DrcProfile
instance ToByteString Ac3DrcProfile
instance ToQuery      Ac3DrcProfile
instance ToHeader     Ac3DrcProfile

instance ToJSON Ac3DrcProfile where
    toJSON = toJSONText

instance FromJSON Ac3DrcProfile where
    parseJSON = parseJSONText "Ac3DrcProfile"
