{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.ThreatIntelSetFormat
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.ThreatIntelSetFormat (
  ThreatIntelSetFormat (
    ..
    , TISFAlienVault
    , TISFFireEye
    , TISFOtxCSV
    , TISFProofPoint
    , TISFStix
    , TISFTxt
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ThreatIntelSetFormat = ThreatIntelSetFormat' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern TISFAlienVault :: ThreatIntelSetFormat
pattern TISFAlienVault = ThreatIntelSetFormat' "ALIEN_VAULT"

pattern TISFFireEye :: ThreatIntelSetFormat
pattern TISFFireEye = ThreatIntelSetFormat' "FIRE_EYE"

pattern TISFOtxCSV :: ThreatIntelSetFormat
pattern TISFOtxCSV = ThreatIntelSetFormat' "OTX_CSV"

pattern TISFProofPoint :: ThreatIntelSetFormat
pattern TISFProofPoint = ThreatIntelSetFormat' "PROOF_POINT"

pattern TISFStix :: ThreatIntelSetFormat
pattern TISFStix = ThreatIntelSetFormat' "STIX"

pattern TISFTxt :: ThreatIntelSetFormat
pattern TISFTxt = ThreatIntelSetFormat' "TXT"

{-# COMPLETE
  TISFAlienVault,
  TISFFireEye,
  TISFOtxCSV,
  TISFProofPoint,
  TISFStix,
  TISFTxt,
  ThreatIntelSetFormat' #-}

instance FromText ThreatIntelSetFormat where
    parser = (ThreatIntelSetFormat' . mk) <$> takeText

instance ToText ThreatIntelSetFormat where
    toText (ThreatIntelSetFormat' ci) = original ci

-- | Represents an enum of /known/ $ThreatIntelSetFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ThreatIntelSetFormat where
    toEnum i = case i of
        0 -> TISFAlienVault
        1 -> TISFFireEye
        2 -> TISFOtxCSV
        3 -> TISFProofPoint
        4 -> TISFStix
        5 -> TISFTxt
        _ -> (error . showText) $ "Unknown index for ThreatIntelSetFormat: " <> toText i
    fromEnum x = case x of
        TISFAlienVault -> 0
        TISFFireEye -> 1
        TISFOtxCSV -> 2
        TISFProofPoint -> 3
        TISFStix -> 4
        TISFTxt -> 5
        ThreatIntelSetFormat' name -> (error . showText) $ "Unknown ThreatIntelSetFormat: " <> original name

-- | Represents the bounds of /known/ $ThreatIntelSetFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ThreatIntelSetFormat where
    minBound = TISFAlienVault
    maxBound = TISFTxt

instance Hashable     ThreatIntelSetFormat
instance NFData       ThreatIntelSetFormat
instance ToByteString ThreatIntelSetFormat
instance ToQuery      ThreatIntelSetFormat
instance ToHeader     ThreatIntelSetFormat

instance ToJSON ThreatIntelSetFormat where
    toJSON = toJSONText

instance FromJSON ThreatIntelSetFormat where
    parseJSON = parseJSONText "ThreatIntelSetFormat"
