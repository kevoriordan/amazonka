{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.IPAddressType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.IPAddressType (
  IPAddressType (
    ..
    , Dualstack
    , IPV4
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data IPAddressType = IPAddressType' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern Dualstack :: IPAddressType
pattern Dualstack = IPAddressType' "dualstack"

pattern IPV4 :: IPAddressType
pattern IPV4 = IPAddressType' "ipv4"

{-# COMPLETE
  Dualstack,
  IPV4,
  IPAddressType' #-}

instance FromText IPAddressType where
    parser = (IPAddressType' . mk) <$> takeText

instance ToText IPAddressType where
    toText (IPAddressType' ci) = original ci

-- | Represents an enum of /known/ $IPAddressType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum IPAddressType where
    toEnum i = case i of
        0 -> Dualstack
        1 -> IPV4
        _ -> (error . showText) $ "Unknown index for IPAddressType: " <> toText i
    fromEnum x = case x of
        Dualstack -> 0
        IPV4 -> 1
        IPAddressType' name -> (error . showText) $ "Unknown IPAddressType: " <> original name

-- | Represents the bounds of /known/ $IPAddressType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded IPAddressType where
    minBound = Dualstack
    maxBound = IPV4

instance Hashable     IPAddressType
instance NFData       IPAddressType
instance ToByteString IPAddressType
instance ToQuery      IPAddressType
instance ToHeader     IPAddressType

instance FromXML IPAddressType where
    parseXML = parseXMLText "IPAddressType"
