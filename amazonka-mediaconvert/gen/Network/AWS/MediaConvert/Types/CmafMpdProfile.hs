{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafMpdProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.CmafMpdProfile (
  CmafMpdProfile (
    ..
    , MainProfile
    , OnDemandProfile
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Specify whether your DASH profile is on-demand or main. When you choose Main profile (MAIN_PROFILE), the service signals  urn:mpeg:dash:profile:isoff-main:2011 in your .mpd DASH manifest. When you choose On-demand (ON_DEMAND_PROFILE), the service signals urn:mpeg:dash:profile:isoff-on-demand:2011 in your .mpd. When you choose On-demand, you must also set the output group setting Segment control (SegmentControl) to Single file (SINGLE_FILE).
data CmafMpdProfile = CmafMpdProfile' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern MainProfile :: CmafMpdProfile
pattern MainProfile = CmafMpdProfile' "MAIN_PROFILE"

pattern OnDemandProfile :: CmafMpdProfile
pattern OnDemandProfile = CmafMpdProfile' "ON_DEMAND_PROFILE"

{-# COMPLETE
  MainProfile,
  OnDemandProfile,
  CmafMpdProfile' #-}

instance FromText CmafMpdProfile where
    parser = (CmafMpdProfile' . mk) <$> takeText

instance ToText CmafMpdProfile where
    toText (CmafMpdProfile' ci) = original ci

-- | Represents an enum of /known/ $CmafMpdProfile.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CmafMpdProfile where
    toEnum i = case i of
        0 -> MainProfile
        1 -> OnDemandProfile
        _ -> (error . showText) $ "Unknown index for CmafMpdProfile: " <> toText i
    fromEnum x = case x of
        MainProfile -> 0
        OnDemandProfile -> 1
        CmafMpdProfile' name -> (error . showText) $ "Unknown CmafMpdProfile: " <> original name

-- | Represents the bounds of /known/ $CmafMpdProfile.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CmafMpdProfile where
    minBound = MainProfile
    maxBound = OnDemandProfile

instance Hashable     CmafMpdProfile
instance NFData       CmafMpdProfile
instance ToByteString CmafMpdProfile
instance ToQuery      CmafMpdProfile
instance ToHeader     CmafMpdProfile

instance ToJSON CmafMpdProfile where
    toJSON = toJSONText

instance FromJSON CmafMpdProfile where
    parseJSON = parseJSONText "CmafMpdProfile"
