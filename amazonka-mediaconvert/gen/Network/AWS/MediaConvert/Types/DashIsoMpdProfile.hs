{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DashIsoMpdProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.DashIsoMpdProfile (
  DashIsoMpdProfile (
    ..
    , DIMPMainProfile
    , DIMPOnDemandProfile
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Specify whether your DASH profile is on-demand or main. When you choose Main profile (MAIN_PROFILE), the service signals  urn:mpeg:dash:profile:isoff-main:2011 in your .mpd DASH manifest. When you choose On-demand (ON_DEMAND_PROFILE), the service signals urn:mpeg:dash:profile:isoff-on-demand:2011 in your .mpd. When you choose On-demand, you must also set the output group setting Segment control (SegmentControl) to Single file (SINGLE_FILE).
data DashIsoMpdProfile = DashIsoMpdProfile' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern DIMPMainProfile :: DashIsoMpdProfile
pattern DIMPMainProfile = DashIsoMpdProfile' "MAIN_PROFILE"

pattern DIMPOnDemandProfile :: DashIsoMpdProfile
pattern DIMPOnDemandProfile = DashIsoMpdProfile' "ON_DEMAND_PROFILE"

{-# COMPLETE
  DIMPMainProfile,
  DIMPOnDemandProfile,
  DashIsoMpdProfile' #-}

instance FromText DashIsoMpdProfile where
    parser = (DashIsoMpdProfile' . mk) <$> takeText

instance ToText DashIsoMpdProfile where
    toText (DashIsoMpdProfile' ci) = original ci

-- | Represents an enum of /known/ $DashIsoMpdProfile.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DashIsoMpdProfile where
    toEnum i = case i of
        0 -> DIMPMainProfile
        1 -> DIMPOnDemandProfile
        _ -> (error . showText) $ "Unknown index for DashIsoMpdProfile: " <> toText i
    fromEnum x = case x of
        DIMPMainProfile -> 0
        DIMPOnDemandProfile -> 1
        DashIsoMpdProfile' name -> (error . showText) $ "Unknown DashIsoMpdProfile: " <> original name

-- | Represents the bounds of /known/ $DashIsoMpdProfile.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DashIsoMpdProfile where
    minBound = DIMPMainProfile
    maxBound = DIMPOnDemandProfile

instance Hashable     DashIsoMpdProfile
instance NFData       DashIsoMpdProfile
instance ToByteString DashIsoMpdProfile
instance ToQuery      DashIsoMpdProfile
instance ToHeader     DashIsoMpdProfile

instance ToJSON DashIsoMpdProfile where
    toJSON = toJSONText

instance FromJSON DashIsoMpdProfile where
    parseJSON = parseJSONText "DashIsoMpdProfile"
