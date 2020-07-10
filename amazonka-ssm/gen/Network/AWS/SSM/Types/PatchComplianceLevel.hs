{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchComplianceLevel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.PatchComplianceLevel (
  PatchComplianceLevel (
    ..
    , Critical
    , High
    , Informational
    , Low
    , Medium
    , Unspecified
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PatchComplianceLevel = PatchComplianceLevel' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern Critical :: PatchComplianceLevel
pattern Critical = PatchComplianceLevel' "CRITICAL"

pattern High :: PatchComplianceLevel
pattern High = PatchComplianceLevel' "HIGH"

pattern Informational :: PatchComplianceLevel
pattern Informational = PatchComplianceLevel' "INFORMATIONAL"

pattern Low :: PatchComplianceLevel
pattern Low = PatchComplianceLevel' "LOW"

pattern Medium :: PatchComplianceLevel
pattern Medium = PatchComplianceLevel' "MEDIUM"

pattern Unspecified :: PatchComplianceLevel
pattern Unspecified = PatchComplianceLevel' "UNSPECIFIED"

{-# COMPLETE
  Critical,
  High,
  Informational,
  Low,
  Medium,
  Unspecified,
  PatchComplianceLevel' #-}

instance FromText PatchComplianceLevel where
    parser = (PatchComplianceLevel' . mk) <$> takeText

instance ToText PatchComplianceLevel where
    toText (PatchComplianceLevel' ci) = original ci

-- | Represents an enum of /known/ $PatchComplianceLevel.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PatchComplianceLevel where
    toEnum i = case i of
        0 -> Critical
        1 -> High
        2 -> Informational
        3 -> Low
        4 -> Medium
        5 -> Unspecified
        _ -> (error . showText) $ "Unknown index for PatchComplianceLevel: " <> toText i
    fromEnum x = case x of
        Critical -> 0
        High -> 1
        Informational -> 2
        Low -> 3
        Medium -> 4
        Unspecified -> 5
        PatchComplianceLevel' name -> (error . showText) $ "Unknown PatchComplianceLevel: " <> original name

-- | Represents the bounds of /known/ $PatchComplianceLevel.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PatchComplianceLevel where
    minBound = Critical
    maxBound = Unspecified

instance Hashable     PatchComplianceLevel
instance NFData       PatchComplianceLevel
instance ToByteString PatchComplianceLevel
instance ToQuery      PatchComplianceLevel
instance ToHeader     PatchComplianceLevel

instance ToJSON PatchComplianceLevel where
    toJSON = toJSONText

instance FromJSON PatchComplianceLevel where
    parseJSON = parseJSONText "PatchComplianceLevel"
