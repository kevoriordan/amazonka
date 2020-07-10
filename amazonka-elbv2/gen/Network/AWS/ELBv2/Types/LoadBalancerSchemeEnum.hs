{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.LoadBalancerSchemeEnum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.LoadBalancerSchemeEnum (
  LoadBalancerSchemeEnum (
    ..
    , Internal
    , InternetFacing
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LoadBalancerSchemeEnum = LoadBalancerSchemeEnum' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern Internal :: LoadBalancerSchemeEnum
pattern Internal = LoadBalancerSchemeEnum' "internal"

pattern InternetFacing :: LoadBalancerSchemeEnum
pattern InternetFacing = LoadBalancerSchemeEnum' "internet-facing"

{-# COMPLETE
  Internal,
  InternetFacing,
  LoadBalancerSchemeEnum' #-}

instance FromText LoadBalancerSchemeEnum where
    parser = (LoadBalancerSchemeEnum' . mk) <$> takeText

instance ToText LoadBalancerSchemeEnum where
    toText (LoadBalancerSchemeEnum' ci) = original ci

-- | Represents an enum of /known/ $LoadBalancerSchemeEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LoadBalancerSchemeEnum where
    toEnum i = case i of
        0 -> Internal
        1 -> InternetFacing
        _ -> (error . showText) $ "Unknown index for LoadBalancerSchemeEnum: " <> toText i
    fromEnum x = case x of
        Internal -> 0
        InternetFacing -> 1
        LoadBalancerSchemeEnum' name -> (error . showText) $ "Unknown LoadBalancerSchemeEnum: " <> original name

-- | Represents the bounds of /known/ $LoadBalancerSchemeEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LoadBalancerSchemeEnum where
    minBound = Internal
    maxBound = InternetFacing

instance Hashable     LoadBalancerSchemeEnum
instance NFData       LoadBalancerSchemeEnum
instance ToByteString LoadBalancerSchemeEnum
instance ToQuery      LoadBalancerSchemeEnum
instance ToHeader     LoadBalancerSchemeEnum

instance FromXML LoadBalancerSchemeEnum where
    parseXML = parseXMLText "LoadBalancerSchemeEnum"
