{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265AlternateTransferFunctionSei
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H265AlternateTransferFunctionSei (
  H265AlternateTransferFunctionSei (
    ..
    , HATFSDisabled
    , HATFSEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Enables Alternate Transfer Function SEI message for outputs using Hybrid Log Gamma (HLG) Electro-Optical Transfer Function (EOTF).
data H265AlternateTransferFunctionSei = H265AlternateTransferFunctionSei' (CI
                                                                             Text)
                                          deriving (Eq, Ord, Read, Show, Data,
                                                    Typeable, Generic)

pattern HATFSDisabled :: H265AlternateTransferFunctionSei
pattern HATFSDisabled = H265AlternateTransferFunctionSei' "DISABLED"

pattern HATFSEnabled :: H265AlternateTransferFunctionSei
pattern HATFSEnabled = H265AlternateTransferFunctionSei' "ENABLED"

{-# COMPLETE
  HATFSDisabled,
  HATFSEnabled,
  H265AlternateTransferFunctionSei' #-}

instance FromText H265AlternateTransferFunctionSei where
    parser = (H265AlternateTransferFunctionSei' . mk) <$> takeText

instance ToText H265AlternateTransferFunctionSei where
    toText (H265AlternateTransferFunctionSei' ci) = original ci

-- | Represents an enum of /known/ $H265AlternateTransferFunctionSei.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H265AlternateTransferFunctionSei where
    toEnum i = case i of
        0 -> HATFSDisabled
        1 -> HATFSEnabled
        _ -> (error . showText) $ "Unknown index for H265AlternateTransferFunctionSei: " <> toText i
    fromEnum x = case x of
        HATFSDisabled -> 0
        HATFSEnabled -> 1
        H265AlternateTransferFunctionSei' name -> (error . showText) $ "Unknown H265AlternateTransferFunctionSei: " <> original name

-- | Represents the bounds of /known/ $H265AlternateTransferFunctionSei.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H265AlternateTransferFunctionSei where
    minBound = HATFSDisabled
    maxBound = HATFSEnabled

instance Hashable     H265AlternateTransferFunctionSei
instance NFData       H265AlternateTransferFunctionSei
instance ToByteString H265AlternateTransferFunctionSei
instance ToQuery      H265AlternateTransferFunctionSei
instance ToHeader     H265AlternateTransferFunctionSei

instance ToJSON H265AlternateTransferFunctionSei where
    toJSON = toJSONText

instance FromJSON H265AlternateTransferFunctionSei where
    parseJSON = parseJSONText "H265AlternateTransferFunctionSei"
