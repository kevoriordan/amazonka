{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FlowLogsResourceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.FlowLogsResourceType (
  FlowLogsResourceType (
    ..
    , FLRTNetworkInterface
    , FLRTSubnet
    , FLRTVPC
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data FlowLogsResourceType = FlowLogsResourceType' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern FLRTNetworkInterface :: FlowLogsResourceType
pattern FLRTNetworkInterface = FlowLogsResourceType' "NetworkInterface"

pattern FLRTSubnet :: FlowLogsResourceType
pattern FLRTSubnet = FlowLogsResourceType' "Subnet"

pattern FLRTVPC :: FlowLogsResourceType
pattern FLRTVPC = FlowLogsResourceType' "VPC"

{-# COMPLETE
  FLRTNetworkInterface,
  FLRTSubnet,
  FLRTVPC,
  FlowLogsResourceType' #-}

instance FromText FlowLogsResourceType where
    parser = (FlowLogsResourceType' . mk) <$> takeText

instance ToText FlowLogsResourceType where
    toText (FlowLogsResourceType' ci) = original ci

-- | Represents an enum of /known/ $FlowLogsResourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FlowLogsResourceType where
    toEnum i = case i of
        0 -> FLRTNetworkInterface
        1 -> FLRTSubnet
        2 -> FLRTVPC
        _ -> (error . showText) $ "Unknown index for FlowLogsResourceType: " <> toText i
    fromEnum x = case x of
        FLRTNetworkInterface -> 0
        FLRTSubnet -> 1
        FLRTVPC -> 2
        FlowLogsResourceType' name -> (error . showText) $ "Unknown FlowLogsResourceType: " <> original name

-- | Represents the bounds of /known/ $FlowLogsResourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FlowLogsResourceType where
    minBound = FLRTNetworkInterface
    maxBound = FLRTVPC

instance Hashable     FlowLogsResourceType
instance NFData       FlowLogsResourceType
instance ToByteString FlowLogsResourceType
instance ToQuery      FlowLogsResourceType
instance ToHeader     FlowLogsResourceType
