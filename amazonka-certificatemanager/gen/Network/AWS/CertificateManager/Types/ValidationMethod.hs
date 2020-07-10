{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.ValidationMethod
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManager.Types.ValidationMethod (
  ValidationMethod (
    ..
    , DNS
    , Email
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ValidationMethod = ValidationMethod' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern DNS :: ValidationMethod
pattern DNS = ValidationMethod' "DNS"

pattern Email :: ValidationMethod
pattern Email = ValidationMethod' "EMAIL"

{-# COMPLETE
  DNS,
  Email,
  ValidationMethod' #-}

instance FromText ValidationMethod where
    parser = (ValidationMethod' . mk) <$> takeText

instance ToText ValidationMethod where
    toText (ValidationMethod' ci) = original ci

-- | Represents an enum of /known/ $ValidationMethod.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ValidationMethod where
    toEnum i = case i of
        0 -> DNS
        1 -> Email
        _ -> (error . showText) $ "Unknown index for ValidationMethod: " <> toText i
    fromEnum x = case x of
        DNS -> 0
        Email -> 1
        ValidationMethod' name -> (error . showText) $ "Unknown ValidationMethod: " <> original name

-- | Represents the bounds of /known/ $ValidationMethod.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ValidationMethod where
    minBound = DNS
    maxBound = Email

instance Hashable     ValidationMethod
instance NFData       ValidationMethod
instance ToByteString ValidationMethod
instance ToQuery      ValidationMethod
instance ToHeader     ValidationMethod

instance ToJSON ValidationMethod where
    toJSON = toJSONText

instance FromJSON ValidationMethod where
    parseJSON = parseJSONText "ValidationMethod"
