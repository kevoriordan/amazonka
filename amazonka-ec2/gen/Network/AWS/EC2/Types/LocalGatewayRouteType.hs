{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LocalGatewayRouteType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LocalGatewayRouteType (
  LocalGatewayRouteType (
    ..
    , LGRTPropagated
    , LGRTStatic
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data LocalGatewayRouteType = LocalGatewayRouteType' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern LGRTPropagated :: LocalGatewayRouteType
pattern LGRTPropagated = LocalGatewayRouteType' "propagated"

pattern LGRTStatic :: LocalGatewayRouteType
pattern LGRTStatic = LocalGatewayRouteType' "static"

{-# COMPLETE
  LGRTPropagated,
  LGRTStatic,
  LocalGatewayRouteType' #-}

instance FromText LocalGatewayRouteType where
    parser = (LocalGatewayRouteType' . mk) <$> takeText

instance ToText LocalGatewayRouteType where
    toText (LocalGatewayRouteType' ci) = original ci

-- | Represents an enum of /known/ $LocalGatewayRouteType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LocalGatewayRouteType where
    toEnum i = case i of
        0 -> LGRTPropagated
        1 -> LGRTStatic
        _ -> (error . showText) $ "Unknown index for LocalGatewayRouteType: " <> toText i
    fromEnum x = case x of
        LGRTPropagated -> 0
        LGRTStatic -> 1
        LocalGatewayRouteType' name -> (error . showText) $ "Unknown LocalGatewayRouteType: " <> original name

-- | Represents the bounds of /known/ $LocalGatewayRouteType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LocalGatewayRouteType where
    minBound = LGRTPropagated
    maxBound = LGRTStatic

instance Hashable     LocalGatewayRouteType
instance NFData       LocalGatewayRouteType
instance ToByteString LocalGatewayRouteType
instance ToQuery      LocalGatewayRouteType
instance ToHeader     LocalGatewayRouteType

instance FromXML LocalGatewayRouteType where
    parseXML = parseXMLText "LocalGatewayRouteType"
