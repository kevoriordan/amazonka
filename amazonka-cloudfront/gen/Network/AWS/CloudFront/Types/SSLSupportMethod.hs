{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.SSLSupportMethod
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.SSLSupportMethod (
  SSLSupportMethod (
    ..
    , SNIOnly
    , VIP
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SSLSupportMethod = SSLSupportMethod' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern SNIOnly :: SSLSupportMethod
pattern SNIOnly = SSLSupportMethod' "sni-only"

pattern VIP :: SSLSupportMethod
pattern VIP = SSLSupportMethod' "vip"

{-# COMPLETE
  SNIOnly,
  VIP,
  SSLSupportMethod' #-}

instance FromText SSLSupportMethod where
    parser = (SSLSupportMethod' . mk) <$> takeText

instance ToText SSLSupportMethod where
    toText (SSLSupportMethod' ci) = original ci

-- | Represents an enum of /known/ $SSLSupportMethod.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SSLSupportMethod where
    toEnum i = case i of
        0 -> SNIOnly
        1 -> VIP
        _ -> (error . showText) $ "Unknown index for SSLSupportMethod: " <> toText i
    fromEnum x = case x of
        SNIOnly -> 0
        VIP -> 1
        SSLSupportMethod' name -> (error . showText) $ "Unknown SSLSupportMethod: " <> original name

-- | Represents the bounds of /known/ $SSLSupportMethod.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SSLSupportMethod where
    minBound = SNIOnly
    maxBound = VIP

instance Hashable     SSLSupportMethod
instance NFData       SSLSupportMethod
instance ToByteString SSLSupportMethod
instance ToQuery      SSLSupportMethod
instance ToHeader     SSLSupportMethod

instance FromXML SSLSupportMethod where
    parseXML = parseXMLText "SSLSupportMethod"

instance ToXML SSLSupportMethod where
    toXML = toXMLText
