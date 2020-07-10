{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.GatewayType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.GatewayType (
  GatewayType (
    ..
    , IPsec_1
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data GatewayType = GatewayType' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern IPsec_1 :: GatewayType
pattern IPsec_1 = GatewayType' "ipsec.1"

{-# COMPLETE
  IPsec_1,
  GatewayType' #-}

instance FromText GatewayType where
    parser = (GatewayType' . mk) <$> takeText

instance ToText GatewayType where
    toText (GatewayType' ci) = original ci

-- | Represents an enum of /known/ $GatewayType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum GatewayType where
    toEnum i = case i of
        0 -> IPsec_1
        _ -> (error . showText) $ "Unknown index for GatewayType: " <> toText i
    fromEnum x = case x of
        IPsec_1 -> 0
        GatewayType' name -> (error . showText) $ "Unknown GatewayType: " <> original name

-- | Represents the bounds of /known/ $GatewayType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded GatewayType where
    minBound = IPsec_1
    maxBound = IPsec_1

instance Hashable     GatewayType
instance NFData       GatewayType
instance ToByteString GatewayType
instance ToQuery      GatewayType
instance ToHeader     GatewayType

instance FromXML GatewayType where
    parseXML = parseXMLText "GatewayType"
