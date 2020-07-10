{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.TLSSecurityPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types.TLSSecurityPolicy (
  TLSSecurityPolicy (
    ..
    , PolicyMinTLS10201907
    , PolicyMinTLS12201907
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The minimum required TLS version.
--
--
data TLSSecurityPolicy = TLSSecurityPolicy' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern PolicyMinTLS10201907 :: TLSSecurityPolicy
pattern PolicyMinTLS10201907 = TLSSecurityPolicy' "Policy-Min-TLS-1-0-2019-07"

pattern PolicyMinTLS12201907 :: TLSSecurityPolicy
pattern PolicyMinTLS12201907 = TLSSecurityPolicy' "Policy-Min-TLS-1-2-2019-07"

{-# COMPLETE
  PolicyMinTLS10201907,
  PolicyMinTLS12201907,
  TLSSecurityPolicy' #-}

instance FromText TLSSecurityPolicy where
    parser = (TLSSecurityPolicy' . mk) <$> takeText

instance ToText TLSSecurityPolicy where
    toText (TLSSecurityPolicy' ci) = original ci

-- | Represents an enum of /known/ $TLSSecurityPolicy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TLSSecurityPolicy where
    toEnum i = case i of
        0 -> PolicyMinTLS10201907
        1 -> PolicyMinTLS12201907
        _ -> (error . showText) $ "Unknown index for TLSSecurityPolicy: " <> toText i
    fromEnum x = case x of
        PolicyMinTLS10201907 -> 0
        PolicyMinTLS12201907 -> 1
        TLSSecurityPolicy' name -> (error . showText) $ "Unknown TLSSecurityPolicy: " <> original name

-- | Represents the bounds of /known/ $TLSSecurityPolicy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TLSSecurityPolicy where
    minBound = PolicyMinTLS10201907
    maxBound = PolicyMinTLS12201907

instance Hashable     TLSSecurityPolicy
instance NFData       TLSSecurityPolicy
instance ToByteString TLSSecurityPolicy
instance ToQuery      TLSSecurityPolicy
instance ToHeader     TLSSecurityPolicy

instance FromXML TLSSecurityPolicy where
    parseXML = parseXMLText "TLSSecurityPolicy"
