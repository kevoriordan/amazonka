{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.IPSetFormat
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.IPSetFormat (
  IPSetFormat (
    ..
    , AlienVault
    , FireEye
    , OtxCSV
    , ProofPoint
    , Stix
    , Txt
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data IPSetFormat = IPSetFormat' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern AlienVault :: IPSetFormat
pattern AlienVault = IPSetFormat' "ALIEN_VAULT"

pattern FireEye :: IPSetFormat
pattern FireEye = IPSetFormat' "FIRE_EYE"

pattern OtxCSV :: IPSetFormat
pattern OtxCSV = IPSetFormat' "OTX_CSV"

pattern ProofPoint :: IPSetFormat
pattern ProofPoint = IPSetFormat' "PROOF_POINT"

pattern Stix :: IPSetFormat
pattern Stix = IPSetFormat' "STIX"

pattern Txt :: IPSetFormat
pattern Txt = IPSetFormat' "TXT"

{-# COMPLETE
  AlienVault,
  FireEye,
  OtxCSV,
  ProofPoint,
  Stix,
  Txt,
  IPSetFormat' #-}

instance FromText IPSetFormat where
    parser = (IPSetFormat' . mk) <$> takeText

instance ToText IPSetFormat where
    toText (IPSetFormat' ci) = original ci

-- | Represents an enum of /known/ $IPSetFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum IPSetFormat where
    toEnum i = case i of
        0 -> AlienVault
        1 -> FireEye
        2 -> OtxCSV
        3 -> ProofPoint
        4 -> Stix
        5 -> Txt
        _ -> (error . showText) $ "Unknown index for IPSetFormat: " <> toText i
    fromEnum x = case x of
        AlienVault -> 0
        FireEye -> 1
        OtxCSV -> 2
        ProofPoint -> 3
        Stix -> 4
        Txt -> 5
        IPSetFormat' name -> (error . showText) $ "Unknown IPSetFormat: " <> original name

-- | Represents the bounds of /known/ $IPSetFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded IPSetFormat where
    minBound = AlienVault
    maxBound = Txt

instance Hashable     IPSetFormat
instance NFData       IPSetFormat
instance ToByteString IPSetFormat
instance ToQuery      IPSetFormat
instance ToHeader     IPSetFormat

instance ToJSON IPSetFormat where
    toJSON = toJSONText

instance FromJSON IPSetFormat where
    parseJSON = parseJSONText "IPSetFormat"
