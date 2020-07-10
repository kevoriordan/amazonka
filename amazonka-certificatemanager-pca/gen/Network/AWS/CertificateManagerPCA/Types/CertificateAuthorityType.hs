{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityType (
  CertificateAuthorityType (
    ..
    , Root
    , Subordinate
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CertificateAuthorityType = CertificateAuthorityType' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern Root :: CertificateAuthorityType
pattern Root = CertificateAuthorityType' "ROOT"

pattern Subordinate :: CertificateAuthorityType
pattern Subordinate = CertificateAuthorityType' "SUBORDINATE"

{-# COMPLETE
  Root,
  Subordinate,
  CertificateAuthorityType' #-}

instance FromText CertificateAuthorityType where
    parser = (CertificateAuthorityType' . mk) <$> takeText

instance ToText CertificateAuthorityType where
    toText (CertificateAuthorityType' ci) = original ci

-- | Represents an enum of /known/ $CertificateAuthorityType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CertificateAuthorityType where
    toEnum i = case i of
        0 -> Root
        1 -> Subordinate
        _ -> (error . showText) $ "Unknown index for CertificateAuthorityType: " <> toText i
    fromEnum x = case x of
        Root -> 0
        Subordinate -> 1
        CertificateAuthorityType' name -> (error . showText) $ "Unknown CertificateAuthorityType: " <> original name

-- | Represents the bounds of /known/ $CertificateAuthorityType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CertificateAuthorityType where
    minBound = Root
    maxBound = Subordinate

instance Hashable     CertificateAuthorityType
instance NFData       CertificateAuthorityType
instance ToByteString CertificateAuthorityType
instance ToQuery      CertificateAuthorityType
instance ToHeader     CertificateAuthorityType

instance ToJSON CertificateAuthorityType where
    toJSON = toJSONText

instance FromJSON CertificateAuthorityType where
    parseJSON = parseJSONText "CertificateAuthorityType"
