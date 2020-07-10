{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfaceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.NetworkInterfaceType (
  NetworkInterfaceType (
    ..
    , NITEfa
    , NITInterface
    , NITNatGateway
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data NetworkInterfaceType = NetworkInterfaceType' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern NITEfa :: NetworkInterfaceType
pattern NITEfa = NetworkInterfaceType' "efa"

pattern NITInterface :: NetworkInterfaceType
pattern NITInterface = NetworkInterfaceType' "interface"

pattern NITNatGateway :: NetworkInterfaceType
pattern NITNatGateway = NetworkInterfaceType' "natGateway"

{-# COMPLETE
  NITEfa,
  NITInterface,
  NITNatGateway,
  NetworkInterfaceType' #-}

instance FromText NetworkInterfaceType where
    parser = (NetworkInterfaceType' . mk) <$> takeText

instance ToText NetworkInterfaceType where
    toText (NetworkInterfaceType' ci) = original ci

-- | Represents an enum of /known/ $NetworkInterfaceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum NetworkInterfaceType where
    toEnum i = case i of
        0 -> NITEfa
        1 -> NITInterface
        2 -> NITNatGateway
        _ -> (error . showText) $ "Unknown index for NetworkInterfaceType: " <> toText i
    fromEnum x = case x of
        NITEfa -> 0
        NITInterface -> 1
        NITNatGateway -> 2
        NetworkInterfaceType' name -> (error . showText) $ "Unknown NetworkInterfaceType: " <> original name

-- | Represents the bounds of /known/ $NetworkInterfaceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded NetworkInterfaceType where
    minBound = NITEfa
    maxBound = NITNatGateway

instance Hashable     NetworkInterfaceType
instance NFData       NetworkInterfaceType
instance ToByteString NetworkInterfaceType
instance ToQuery      NetworkInterfaceType
instance ToHeader     NetworkInterfaceType

instance FromXML NetworkInterfaceType where
    parseXML = parseXMLText "NetworkInterfaceType"
