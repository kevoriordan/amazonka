{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CertificateSource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.CertificateSource (
  CertificateSource (
    ..
    , Acm
    , Cloudfront
    , IAM
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CertificateSource = CertificateSource' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern Acm :: CertificateSource
pattern Acm = CertificateSource' "acm"

pattern Cloudfront :: CertificateSource
pattern Cloudfront = CertificateSource' "cloudfront"

pattern IAM :: CertificateSource
pattern IAM = CertificateSource' "iam"

{-# COMPLETE
  Acm,
  Cloudfront,
  IAM,
  CertificateSource' #-}

instance FromText CertificateSource where
    parser = (CertificateSource' . mk) <$> takeText

instance ToText CertificateSource where
    toText (CertificateSource' ci) = original ci

-- | Represents an enum of /known/ $CertificateSource.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CertificateSource where
    toEnum i = case i of
        0 -> Acm
        1 -> Cloudfront
        2 -> IAM
        _ -> (error . showText) $ "Unknown index for CertificateSource: " <> toText i
    fromEnum x = case x of
        Acm -> 0
        Cloudfront -> 1
        IAM -> 2
        CertificateSource' name -> (error . showText) $ "Unknown CertificateSource: " <> original name

-- | Represents the bounds of /known/ $CertificateSource.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CertificateSource where
    minBound = Acm
    maxBound = IAM

instance Hashable     CertificateSource
instance NFData       CertificateSource
instance ToByteString CertificateSource
instance ToQuery      CertificateSource
instance ToHeader     CertificateSource

instance FromXML CertificateSource where
    parseXML = parseXMLText "CertificateSource"

instance ToXML CertificateSource where
    toXML = toXMLText
