{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayRouteType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TransitGatewayRouteType (
  TransitGatewayRouteType (
    ..
    , TGRTPropagated
    , TGRTStatic
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TransitGatewayRouteType = TransitGatewayRouteType' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern TGRTPropagated :: TransitGatewayRouteType
pattern TGRTPropagated = TransitGatewayRouteType' "propagated"

pattern TGRTStatic :: TransitGatewayRouteType
pattern TGRTStatic = TransitGatewayRouteType' "static"

{-# COMPLETE
  TGRTPropagated,
  TGRTStatic,
  TransitGatewayRouteType' #-}

instance FromText TransitGatewayRouteType where
    parser = (TransitGatewayRouteType' . mk) <$> takeText

instance ToText TransitGatewayRouteType where
    toText (TransitGatewayRouteType' ci) = original ci

-- | Represents an enum of /known/ $TransitGatewayRouteType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TransitGatewayRouteType where
    toEnum i = case i of
        0 -> TGRTPropagated
        1 -> TGRTStatic
        _ -> (error . showText) $ "Unknown index for TransitGatewayRouteType: " <> toText i
    fromEnum x = case x of
        TGRTPropagated -> 0
        TGRTStatic -> 1
        TransitGatewayRouteType' name -> (error . showText) $ "Unknown TransitGatewayRouteType: " <> original name

-- | Represents the bounds of /known/ $TransitGatewayRouteType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TransitGatewayRouteType where
    minBound = TGRTPropagated
    maxBound = TGRTStatic

instance Hashable     TransitGatewayRouteType
instance NFData       TransitGatewayRouteType
instance ToByteString TransitGatewayRouteType
instance ToQuery      TransitGatewayRouteType
instance ToHeader     TransitGatewayRouteType

instance FromXML TransitGatewayRouteType where
    parseXML = parseXMLText "TransitGatewayRouteType"
