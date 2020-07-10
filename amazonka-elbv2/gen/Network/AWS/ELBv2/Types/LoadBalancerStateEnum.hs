{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.LoadBalancerStateEnum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.LoadBalancerStateEnum (
  LoadBalancerStateEnum (
    ..
    , Active
    , ActiveImpaired
    , Failed
    , Provisioning
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LoadBalancerStateEnum = LoadBalancerStateEnum' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern Active :: LoadBalancerStateEnum
pattern Active = LoadBalancerStateEnum' "active"

pattern ActiveImpaired :: LoadBalancerStateEnum
pattern ActiveImpaired = LoadBalancerStateEnum' "active_impaired"

pattern Failed :: LoadBalancerStateEnum
pattern Failed = LoadBalancerStateEnum' "failed"

pattern Provisioning :: LoadBalancerStateEnum
pattern Provisioning = LoadBalancerStateEnum' "provisioning"

{-# COMPLETE
  Active,
  ActiveImpaired,
  Failed,
  Provisioning,
  LoadBalancerStateEnum' #-}

instance FromText LoadBalancerStateEnum where
    parser = (LoadBalancerStateEnum' . mk) <$> takeText

instance ToText LoadBalancerStateEnum where
    toText (LoadBalancerStateEnum' ci) = original ci

-- | Represents an enum of /known/ $LoadBalancerStateEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LoadBalancerStateEnum where
    toEnum i = case i of
        0 -> Active
        1 -> ActiveImpaired
        2 -> Failed
        3 -> Provisioning
        _ -> (error . showText) $ "Unknown index for LoadBalancerStateEnum: " <> toText i
    fromEnum x = case x of
        Active -> 0
        ActiveImpaired -> 1
        Failed -> 2
        Provisioning -> 3
        LoadBalancerStateEnum' name -> (error . showText) $ "Unknown LoadBalancerStateEnum: " <> original name

-- | Represents the bounds of /known/ $LoadBalancerStateEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LoadBalancerStateEnum where
    minBound = Active
    maxBound = Provisioning

instance Hashable     LoadBalancerStateEnum
instance NFData       LoadBalancerStateEnum
instance ToByteString LoadBalancerStateEnum
instance ToQuery      LoadBalancerStateEnum
instance ToHeader     LoadBalancerStateEnum

instance FromXML LoadBalancerStateEnum where
    parseXML = parseXMLText "LoadBalancerStateEnum"
