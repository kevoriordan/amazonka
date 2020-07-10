{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ServiceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ServiceType (
  ServiceType (
    ..
    , Gateway
    , Interface
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ServiceType = ServiceType' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Gateway :: ServiceType
pattern Gateway = ServiceType' "Gateway"

pattern Interface :: ServiceType
pattern Interface = ServiceType' "Interface"

{-# COMPLETE
  Gateway,
  Interface,
  ServiceType' #-}

instance FromText ServiceType where
    parser = (ServiceType' . mk) <$> takeText

instance ToText ServiceType where
    toText (ServiceType' ci) = original ci

-- | Represents an enum of /known/ $ServiceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ServiceType where
    toEnum i = case i of
        0 -> Gateway
        1 -> Interface
        _ -> (error . showText) $ "Unknown index for ServiceType: " <> toText i
    fromEnum x = case x of
        Gateway -> 0
        Interface -> 1
        ServiceType' name -> (error . showText) $ "Unknown ServiceType: " <> original name

-- | Represents the bounds of /known/ $ServiceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ServiceType where
    minBound = Gateway
    maxBound = Interface

instance Hashable     ServiceType
instance NFData       ServiceType
instance ToByteString ServiceType
instance ToQuery      ServiceType
instance ToHeader     ServiceType

instance FromXML ServiceType where
    parseXML = parseXMLText "ServiceType"
