{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CertificateMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.CertificateMode (
  CertificateMode (
    ..
    , Default
    , SNIOnly
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CertificateMode = CertificateMode' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Default :: CertificateMode
pattern Default = CertificateMode' "DEFAULT"

pattern SNIOnly :: CertificateMode
pattern SNIOnly = CertificateMode' "SNI_ONLY"

{-# COMPLETE
  Default,
  SNIOnly,
  CertificateMode' #-}

instance FromText CertificateMode where
    parser = (CertificateMode' . mk) <$> takeText

instance ToText CertificateMode where
    toText (CertificateMode' ci) = original ci

-- | Represents an enum of /known/ $CertificateMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CertificateMode where
    toEnum i = case i of
        0 -> Default
        1 -> SNIOnly
        _ -> (error . showText) $ "Unknown index for CertificateMode: " <> toText i
    fromEnum x = case x of
        Default -> 0
        SNIOnly -> 1
        CertificateMode' name -> (error . showText) $ "Unknown CertificateMode: " <> original name

-- | Represents the bounds of /known/ $CertificateMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CertificateMode where
    minBound = Default
    maxBound = SNIOnly

instance Hashable     CertificateMode
instance NFData       CertificateMode
instance ToByteString CertificateMode
instance ToQuery      CertificateMode
instance ToHeader     CertificateMode

instance FromJSON CertificateMode where
    parseJSON = parseJSONText "CertificateMode"
