{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.LoadBalancerTypeEnum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.LoadBalancerTypeEnum (
  LoadBalancerTypeEnum (
    ..
    , Application
    , Network
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LoadBalancerTypeEnum = LoadBalancerTypeEnum' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern Application :: LoadBalancerTypeEnum
pattern Application = LoadBalancerTypeEnum' "application"

pattern Network :: LoadBalancerTypeEnum
pattern Network = LoadBalancerTypeEnum' "network"

{-# COMPLETE
  Application,
  Network,
  LoadBalancerTypeEnum' #-}

instance FromText LoadBalancerTypeEnum where
    parser = (LoadBalancerTypeEnum' . mk) <$> takeText

instance ToText LoadBalancerTypeEnum where
    toText (LoadBalancerTypeEnum' ci) = original ci

-- | Represents an enum of /known/ $LoadBalancerTypeEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LoadBalancerTypeEnum where
    toEnum i = case i of
        0 -> Application
        1 -> Network
        _ -> (error . showText) $ "Unknown index for LoadBalancerTypeEnum: " <> toText i
    fromEnum x = case x of
        Application -> 0
        Network -> 1
        LoadBalancerTypeEnum' name -> (error . showText) $ "Unknown LoadBalancerTypeEnum: " <> original name

-- | Represents the bounds of /known/ $LoadBalancerTypeEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LoadBalancerTypeEnum where
    minBound = Application
    maxBound = Network

instance Hashable     LoadBalancerTypeEnum
instance NFData       LoadBalancerTypeEnum
instance ToByteString LoadBalancerTypeEnum
instance ToQuery      LoadBalancerTypeEnum
instance ToHeader     LoadBalancerTypeEnum

instance FromXML LoadBalancerTypeEnum where
    parseXML = parseXMLText "LoadBalancerTypeEnum"
