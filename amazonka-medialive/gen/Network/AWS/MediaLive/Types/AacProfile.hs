{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AacProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.AacProfile (
  AacProfile (
    ..
    , APHEV1
    , APHEV2
    , APLC
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Aac Profile
data AacProfile = AacProfile' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern APHEV1 :: AacProfile
pattern APHEV1 = AacProfile' "HEV1"

pattern APHEV2 :: AacProfile
pattern APHEV2 = AacProfile' "HEV2"

pattern APLC :: AacProfile
pattern APLC = AacProfile' "LC"

{-# COMPLETE
  APHEV1,
  APHEV2,
  APLC,
  AacProfile' #-}

instance FromText AacProfile where
    parser = (AacProfile' . mk) <$> takeText

instance ToText AacProfile where
    toText (AacProfile' ci) = original ci

-- | Represents an enum of /known/ $AacProfile.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AacProfile where
    toEnum i = case i of
        0 -> APHEV1
        1 -> APHEV2
        2 -> APLC
        _ -> (error . showText) $ "Unknown index for AacProfile: " <> toText i
    fromEnum x = case x of
        APHEV1 -> 0
        APHEV2 -> 1
        APLC -> 2
        AacProfile' name -> (error . showText) $ "Unknown AacProfile: " <> original name

-- | Represents the bounds of /known/ $AacProfile.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AacProfile where
    minBound = APHEV1
    maxBound = APLC

instance Hashable     AacProfile
instance NFData       AacProfile
instance ToByteString AacProfile
instance ToQuery      AacProfile
instance ToHeader     AacProfile

instance ToJSON AacProfile where
    toJSON = toJSONText

instance FromJSON AacProfile where
    parseJSON = parseJSONText "AacProfile"
