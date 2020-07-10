{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.CertificateTransparencyLoggingPreference
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManager.Types.CertificateTransparencyLoggingPreference (
  CertificateTransparencyLoggingPreference (
    ..
    , Disabled
    , Enabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CertificateTransparencyLoggingPreference = CertificateTransparencyLoggingPreference' (CI
                                                                                             Text)
                                                  deriving (Eq, Ord, Read, Show,
                                                            Data, Typeable,
                                                            Generic)

pattern Disabled :: CertificateTransparencyLoggingPreference
pattern Disabled = CertificateTransparencyLoggingPreference' "DISABLED"

pattern Enabled :: CertificateTransparencyLoggingPreference
pattern Enabled = CertificateTransparencyLoggingPreference' "ENABLED"

{-# COMPLETE
  Disabled,
  Enabled,
  CertificateTransparencyLoggingPreference' #-}

instance FromText CertificateTransparencyLoggingPreference where
    parser = (CertificateTransparencyLoggingPreference' . mk) <$> takeText

instance ToText CertificateTransparencyLoggingPreference where
    toText (CertificateTransparencyLoggingPreference' ci) = original ci

-- | Represents an enum of /known/ $CertificateTransparencyLoggingPreference.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CertificateTransparencyLoggingPreference where
    toEnum i = case i of
        0 -> Disabled
        1 -> Enabled
        _ -> (error . showText) $ "Unknown index for CertificateTransparencyLoggingPreference: " <> toText i
    fromEnum x = case x of
        Disabled -> 0
        Enabled -> 1
        CertificateTransparencyLoggingPreference' name -> (error . showText) $ "Unknown CertificateTransparencyLoggingPreference: " <> original name

-- | Represents the bounds of /known/ $CertificateTransparencyLoggingPreference.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CertificateTransparencyLoggingPreference where
    minBound = Disabled
    maxBound = Enabled

instance Hashable     CertificateTransparencyLoggingPreference
instance NFData       CertificateTransparencyLoggingPreference
instance ToByteString CertificateTransparencyLoggingPreference
instance ToQuery      CertificateTransparencyLoggingPreference
instance ToHeader     CertificateTransparencyLoggingPreference

instance ToJSON CertificateTransparencyLoggingPreference where
    toJSON = toJSONText

instance FromJSON CertificateTransparencyLoggingPreference where
    parseJSON = parseJSONText "CertificateTransparencyLoggingPreference"
