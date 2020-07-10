{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.CertificateType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.CertificateType (
  CertificateType (
    ..
    , Disabled
    , Generated
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CertificateType = CertificateType' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Disabled :: CertificateType
pattern Disabled = CertificateType' "DISABLED"

pattern Generated :: CertificateType
pattern Generated = CertificateType' "GENERATED"

{-# COMPLETE
  Disabled,
  Generated,
  CertificateType' #-}

instance FromText CertificateType where
    parser = (CertificateType' . mk) <$> takeText

instance ToText CertificateType where
    toText (CertificateType' ci) = original ci

-- | Represents an enum of /known/ $CertificateType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CertificateType where
    toEnum i = case i of
        0 -> Disabled
        1 -> Generated
        _ -> (error . showText) $ "Unknown index for CertificateType: " <> toText i
    fromEnum x = case x of
        Disabled -> 0
        Generated -> 1
        CertificateType' name -> (error . showText) $ "Unknown CertificateType: " <> original name

-- | Represents the bounds of /known/ $CertificateType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CertificateType where
    minBound = Disabled
    maxBound = Generated

instance Hashable     CertificateType
instance NFData       CertificateType
instance ToByteString CertificateType
instance ToQuery      CertificateType
instance ToHeader     CertificateType

instance ToJSON CertificateType where
    toJSON = toJSONText

instance FromJSON CertificateType where
    parseJSON = parseJSONText "CertificateType"
