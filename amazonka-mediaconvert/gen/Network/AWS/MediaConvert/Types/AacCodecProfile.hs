{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AacCodecProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AacCodecProfile (
  AacCodecProfile (
    ..
    , ACPHEV1
    , ACPHEV2
    , ACPLC
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | AAC Profile.
data AacCodecProfile = AacCodecProfile' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern ACPHEV1 :: AacCodecProfile
pattern ACPHEV1 = AacCodecProfile' "HEV1"

pattern ACPHEV2 :: AacCodecProfile
pattern ACPHEV2 = AacCodecProfile' "HEV2"

pattern ACPLC :: AacCodecProfile
pattern ACPLC = AacCodecProfile' "LC"

{-# COMPLETE
  ACPHEV1,
  ACPHEV2,
  ACPLC,
  AacCodecProfile' #-}

instance FromText AacCodecProfile where
    parser = (AacCodecProfile' . mk) <$> takeText

instance ToText AacCodecProfile where
    toText (AacCodecProfile' ci) = original ci

-- | Represents an enum of /known/ $AacCodecProfile.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AacCodecProfile where
    toEnum i = case i of
        0 -> ACPHEV1
        1 -> ACPHEV2
        2 -> ACPLC
        _ -> (error . showText) $ "Unknown index for AacCodecProfile: " <> toText i
    fromEnum x = case x of
        ACPHEV1 -> 0
        ACPHEV2 -> 1
        ACPLC -> 2
        AacCodecProfile' name -> (error . showText) $ "Unknown AacCodecProfile: " <> original name

-- | Represents the bounds of /known/ $AacCodecProfile.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AacCodecProfile where
    minBound = ACPHEV1
    maxBound = ACPLC

instance Hashable     AacCodecProfile
instance NFData       AacCodecProfile
instance ToByteString AacCodecProfile
instance ToQuery      AacCodecProfile
instance ToHeader     AacCodecProfile

instance ToJSON AacCodecProfile where
    toJSON = toJSONText

instance FromJSON AacCodecProfile where
    parseJSON = parseJSONText "AacCodecProfile"
