{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ResourceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.ResourceType (
  ResourceType (
    ..
    , RTAlarm
    , RTCloudFormationStackRecord
    , RTContactMethod
    , RTDisk
    , RTDiskSnapshot
    , RTDomain
    , RTExportSnapshotRecord
    , RTInstance
    , RTInstanceSnapshot
    , RTKeyPair
    , RTLoadBalancer
    , RTLoadBalancerTLSCertificate
    , RTPeeredVPC
    , RTRelationalDatabase
    , RTRelationalDatabaseSnapshot
    , RTStaticIP
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ResourceType = ResourceType' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern RTAlarm :: ResourceType
pattern RTAlarm = ResourceType' "Alarm"

pattern RTCloudFormationStackRecord :: ResourceType
pattern RTCloudFormationStackRecord = ResourceType' "CloudFormationStackRecord"

pattern RTContactMethod :: ResourceType
pattern RTContactMethod = ResourceType' "ContactMethod"

pattern RTDisk :: ResourceType
pattern RTDisk = ResourceType' "Disk"

pattern RTDiskSnapshot :: ResourceType
pattern RTDiskSnapshot = ResourceType' "DiskSnapshot"

pattern RTDomain :: ResourceType
pattern RTDomain = ResourceType' "Domain"

pattern RTExportSnapshotRecord :: ResourceType
pattern RTExportSnapshotRecord = ResourceType' "ExportSnapshotRecord"

pattern RTInstance :: ResourceType
pattern RTInstance = ResourceType' "Instance"

pattern RTInstanceSnapshot :: ResourceType
pattern RTInstanceSnapshot = ResourceType' "InstanceSnapshot"

pattern RTKeyPair :: ResourceType
pattern RTKeyPair = ResourceType' "KeyPair"

pattern RTLoadBalancer :: ResourceType
pattern RTLoadBalancer = ResourceType' "LoadBalancer"

pattern RTLoadBalancerTLSCertificate :: ResourceType
pattern RTLoadBalancerTLSCertificate = ResourceType' "LoadBalancerTlsCertificate"

pattern RTPeeredVPC :: ResourceType
pattern RTPeeredVPC = ResourceType' "PeeredVpc"

pattern RTRelationalDatabase :: ResourceType
pattern RTRelationalDatabase = ResourceType' "RelationalDatabase"

pattern RTRelationalDatabaseSnapshot :: ResourceType
pattern RTRelationalDatabaseSnapshot = ResourceType' "RelationalDatabaseSnapshot"

pattern RTStaticIP :: ResourceType
pattern RTStaticIP = ResourceType' "StaticIp"

{-# COMPLETE
  RTAlarm,
  RTCloudFormationStackRecord,
  RTContactMethod,
  RTDisk,
  RTDiskSnapshot,
  RTDomain,
  RTExportSnapshotRecord,
  RTInstance,
  RTInstanceSnapshot,
  RTKeyPair,
  RTLoadBalancer,
  RTLoadBalancerTLSCertificate,
  RTPeeredVPC,
  RTRelationalDatabase,
  RTRelationalDatabaseSnapshot,
  RTStaticIP,
  ResourceType' #-}

instance FromText ResourceType where
    parser = (ResourceType' . mk) <$> takeText

instance ToText ResourceType where
    toText (ResourceType' ci) = original ci

-- | Represents an enum of /known/ $ResourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ResourceType where
    toEnum i = case i of
        0 -> RTAlarm
        1 -> RTCloudFormationStackRecord
        2 -> RTContactMethod
        3 -> RTDisk
        4 -> RTDiskSnapshot
        5 -> RTDomain
        6 -> RTExportSnapshotRecord
        7 -> RTInstance
        8 -> RTInstanceSnapshot
        9 -> RTKeyPair
        10 -> RTLoadBalancer
        11 -> RTLoadBalancerTLSCertificate
        12 -> RTPeeredVPC
        13 -> RTRelationalDatabase
        14 -> RTRelationalDatabaseSnapshot
        15 -> RTStaticIP
        _ -> (error . showText) $ "Unknown index for ResourceType: " <> toText i
    fromEnum x = case x of
        RTAlarm -> 0
        RTCloudFormationStackRecord -> 1
        RTContactMethod -> 2
        RTDisk -> 3
        RTDiskSnapshot -> 4
        RTDomain -> 5
        RTExportSnapshotRecord -> 6
        RTInstance -> 7
        RTInstanceSnapshot -> 8
        RTKeyPair -> 9
        RTLoadBalancer -> 10
        RTLoadBalancerTLSCertificate -> 11
        RTPeeredVPC -> 12
        RTRelationalDatabase -> 13
        RTRelationalDatabaseSnapshot -> 14
        RTStaticIP -> 15
        ResourceType' name -> (error . showText) $ "Unknown ResourceType: " <> original name

-- | Represents the bounds of /known/ $ResourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ResourceType where
    minBound = RTAlarm
    maxBound = RTStaticIP

instance Hashable     ResourceType
instance NFData       ResourceType
instance ToByteString ResourceType
instance ToQuery      ResourceType
instance ToHeader     ResourceType

instance FromJSON ResourceType where
    parseJSON = parseJSONText "ResourceType"
