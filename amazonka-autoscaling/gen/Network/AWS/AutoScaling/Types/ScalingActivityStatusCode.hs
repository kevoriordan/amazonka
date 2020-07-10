{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.ScalingActivityStatusCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.ScalingActivityStatusCode (
  ScalingActivityStatusCode (
    ..
    , Cancelled
    , Failed
    , InProgress
    , MidLifecycleAction
    , PendingSpotBidPlacement
    , PreInService
    , Successful
    , WaitingForELBConnectionDraining
    , WaitingForInstanceId
    , WaitingForInstanceWarmup
    , WaitingForSpotInstanceId
    , WaitingForSpotInstanceRequestId
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ScalingActivityStatusCode = ScalingActivityStatusCode' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern Cancelled :: ScalingActivityStatusCode
pattern Cancelled = ScalingActivityStatusCode' "Cancelled"

pattern Failed :: ScalingActivityStatusCode
pattern Failed = ScalingActivityStatusCode' "Failed"

pattern InProgress :: ScalingActivityStatusCode
pattern InProgress = ScalingActivityStatusCode' "InProgress"

pattern MidLifecycleAction :: ScalingActivityStatusCode
pattern MidLifecycleAction = ScalingActivityStatusCode' "MidLifecycleAction"

pattern PendingSpotBidPlacement :: ScalingActivityStatusCode
pattern PendingSpotBidPlacement = ScalingActivityStatusCode' "PendingSpotBidPlacement"

pattern PreInService :: ScalingActivityStatusCode
pattern PreInService = ScalingActivityStatusCode' "PreInService"

pattern Successful :: ScalingActivityStatusCode
pattern Successful = ScalingActivityStatusCode' "Successful"

pattern WaitingForELBConnectionDraining :: ScalingActivityStatusCode
pattern WaitingForELBConnectionDraining = ScalingActivityStatusCode' "WaitingForELBConnectionDraining"

pattern WaitingForInstanceId :: ScalingActivityStatusCode
pattern WaitingForInstanceId = ScalingActivityStatusCode' "WaitingForInstanceId"

pattern WaitingForInstanceWarmup :: ScalingActivityStatusCode
pattern WaitingForInstanceWarmup = ScalingActivityStatusCode' "WaitingForInstanceWarmup"

pattern WaitingForSpotInstanceId :: ScalingActivityStatusCode
pattern WaitingForSpotInstanceId = ScalingActivityStatusCode' "WaitingForSpotInstanceId"

pattern WaitingForSpotInstanceRequestId :: ScalingActivityStatusCode
pattern WaitingForSpotInstanceRequestId = ScalingActivityStatusCode' "WaitingForSpotInstanceRequestId"

{-# COMPLETE
  Cancelled,
  Failed,
  InProgress,
  MidLifecycleAction,
  PendingSpotBidPlacement,
  PreInService,
  Successful,
  WaitingForELBConnectionDraining,
  WaitingForInstanceId,
  WaitingForInstanceWarmup,
  WaitingForSpotInstanceId,
  WaitingForSpotInstanceRequestId,
  ScalingActivityStatusCode' #-}

instance FromText ScalingActivityStatusCode where
    parser = (ScalingActivityStatusCode' . mk) <$> takeText

instance ToText ScalingActivityStatusCode where
    toText (ScalingActivityStatusCode' ci) = original ci

-- | Represents an enum of /known/ $ScalingActivityStatusCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ScalingActivityStatusCode where
    toEnum i = case i of
        0 -> Cancelled
        1 -> Failed
        2 -> InProgress
        3 -> MidLifecycleAction
        4 -> PendingSpotBidPlacement
        5 -> PreInService
        6 -> Successful
        7 -> WaitingForELBConnectionDraining
        8 -> WaitingForInstanceId
        9 -> WaitingForInstanceWarmup
        10 -> WaitingForSpotInstanceId
        11 -> WaitingForSpotInstanceRequestId
        _ -> (error . showText) $ "Unknown index for ScalingActivityStatusCode: " <> toText i
    fromEnum x = case x of
        Cancelled -> 0
        Failed -> 1
        InProgress -> 2
        MidLifecycleAction -> 3
        PendingSpotBidPlacement -> 4
        PreInService -> 5
        Successful -> 6
        WaitingForELBConnectionDraining -> 7
        WaitingForInstanceId -> 8
        WaitingForInstanceWarmup -> 9
        WaitingForSpotInstanceId -> 10
        WaitingForSpotInstanceRequestId -> 11
        ScalingActivityStatusCode' name -> (error . showText) $ "Unknown ScalingActivityStatusCode: " <> original name

-- | Represents the bounds of /known/ $ScalingActivityStatusCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ScalingActivityStatusCode where
    minBound = Cancelled
    maxBound = WaitingForSpotInstanceRequestId

instance Hashable     ScalingActivityStatusCode
instance NFData       ScalingActivityStatusCode
instance ToByteString ScalingActivityStatusCode
instance ToQuery      ScalingActivityStatusCode
instance ToHeader     ScalingActivityStatusCode

instance FromXML ScalingActivityStatusCode where
    parseXML = parseXMLText "ScalingActivityStatusCode"
