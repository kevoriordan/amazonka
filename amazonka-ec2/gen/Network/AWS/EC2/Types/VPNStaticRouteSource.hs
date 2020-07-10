{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPNStaticRouteSource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VPNStaticRouteSource (
  VPNStaticRouteSource (
    ..
    , Static
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VPNStaticRouteSource = VPNStaticRouteSource' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern Static :: VPNStaticRouteSource
pattern Static = VPNStaticRouteSource' "Static"

{-# COMPLETE
  Static,
  VPNStaticRouteSource' #-}

instance FromText VPNStaticRouteSource where
    parser = (VPNStaticRouteSource' . mk) <$> takeText

instance ToText VPNStaticRouteSource where
    toText (VPNStaticRouteSource' ci) = original ci

-- | Represents an enum of /known/ $VPNStaticRouteSource.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum VPNStaticRouteSource where
    toEnum i = case i of
        0 -> Static
        _ -> (error . showText) $ "Unknown index for VPNStaticRouteSource: " <> toText i
    fromEnum x = case x of
        Static -> 0
        VPNStaticRouteSource' name -> (error . showText) $ "Unknown VPNStaticRouteSource: " <> original name

-- | Represents the bounds of /known/ $VPNStaticRouteSource.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded VPNStaticRouteSource where
    minBound = Static
    maxBound = Static

instance Hashable     VPNStaticRouteSource
instance NFData       VPNStaticRouteSource
instance ToByteString VPNStaticRouteSource
instance ToQuery      VPNStaticRouteSource
instance ToHeader     VPNStaticRouteSource

instance FromXML VPNStaticRouteSource where
    parseXML = parseXMLText "VPNStaticRouteSource"
