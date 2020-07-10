{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.OperationType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.OperationType (
  OperationType (
    ..
    , AllocateStaticIP
    , AttachDisk
    , AttachInstancesToLoadBalancer
    , AttachLoadBalancerTLSCertificate
    , AttachStaticIP
    , CloseInstancePublicPorts
    , CreateContactMethod
    , CreateDisk
    , CreateDiskFromSnapshot
    , CreateDiskSnapshot
    , CreateDomain
    , CreateInstance
    , CreateInstanceSnapshot
    , CreateInstancesFromSnapshot
    , CreateLoadBalancer
    , CreateLoadBalancerTLSCertificate
    , CreateRelationalDatabase
    , CreateRelationalDatabaseFromSnapshot
    , CreateRelationalDatabaseSnapshot
    , DeleteAlarm
    , DeleteContactMethod
    , DeleteDisk
    , DeleteDiskSnapshot
    , DeleteDomain
    , DeleteDomainEntry
    , DeleteInstance
    , DeleteInstanceSnapshot
    , DeleteKnownHostKeys
    , DeleteLoadBalancer
    , DeleteLoadBalancerTLSCertificate
    , DeleteRelationalDatabase
    , DeleteRelationalDatabaseSnapshot
    , DetachDisk
    , DetachInstancesFromLoadBalancer
    , DetachStaticIP
    , DisableAddOn
    , EnableAddOn
    , GetAlarms
    , GetContactMethods
    , OpenInstancePublicPorts
    , PutAlarm
    , PutInstancePublicPorts
    , RebootInstance
    , RebootRelationalDatabase
    , ReleaseStaticIP
    , SendContactMethodVerification
    , StartInstance
    , StartRelationalDatabase
    , StopInstance
    , StopRelationalDatabase
    , TestAlarm
    , UpdateDomainEntry
    , UpdateLoadBalancerAttribute
    , UpdateRelationalDatabase
    , UpdateRelationalDatabaseParameters
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data OperationType = OperationType' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern AllocateStaticIP :: OperationType
pattern AllocateStaticIP = OperationType' "AllocateStaticIp"

pattern AttachDisk :: OperationType
pattern AttachDisk = OperationType' "AttachDisk"

pattern AttachInstancesToLoadBalancer :: OperationType
pattern AttachInstancesToLoadBalancer = OperationType' "AttachInstancesToLoadBalancer"

pattern AttachLoadBalancerTLSCertificate :: OperationType
pattern AttachLoadBalancerTLSCertificate = OperationType' "AttachLoadBalancerTlsCertificate"

pattern AttachStaticIP :: OperationType
pattern AttachStaticIP = OperationType' "AttachStaticIp"

pattern CloseInstancePublicPorts :: OperationType
pattern CloseInstancePublicPorts = OperationType' "CloseInstancePublicPorts"

pattern CreateContactMethod :: OperationType
pattern CreateContactMethod = OperationType' "CreateContactMethod"

pattern CreateDisk :: OperationType
pattern CreateDisk = OperationType' "CreateDisk"

pattern CreateDiskFromSnapshot :: OperationType
pattern CreateDiskFromSnapshot = OperationType' "CreateDiskFromSnapshot"

pattern CreateDiskSnapshot :: OperationType
pattern CreateDiskSnapshot = OperationType' "CreateDiskSnapshot"

pattern CreateDomain :: OperationType
pattern CreateDomain = OperationType' "CreateDomain"

pattern CreateInstance :: OperationType
pattern CreateInstance = OperationType' "CreateInstance"

pattern CreateInstanceSnapshot :: OperationType
pattern CreateInstanceSnapshot = OperationType' "CreateInstanceSnapshot"

pattern CreateInstancesFromSnapshot :: OperationType
pattern CreateInstancesFromSnapshot = OperationType' "CreateInstancesFromSnapshot"

pattern CreateLoadBalancer :: OperationType
pattern CreateLoadBalancer = OperationType' "CreateLoadBalancer"

pattern CreateLoadBalancerTLSCertificate :: OperationType
pattern CreateLoadBalancerTLSCertificate = OperationType' "CreateLoadBalancerTlsCertificate"

pattern CreateRelationalDatabase :: OperationType
pattern CreateRelationalDatabase = OperationType' "CreateRelationalDatabase"

pattern CreateRelationalDatabaseFromSnapshot :: OperationType
pattern CreateRelationalDatabaseFromSnapshot = OperationType' "CreateRelationalDatabaseFromSnapshot"

pattern CreateRelationalDatabaseSnapshot :: OperationType
pattern CreateRelationalDatabaseSnapshot = OperationType' "CreateRelationalDatabaseSnapshot"

pattern DeleteAlarm :: OperationType
pattern DeleteAlarm = OperationType' "DeleteAlarm"

pattern DeleteContactMethod :: OperationType
pattern DeleteContactMethod = OperationType' "DeleteContactMethod"

pattern DeleteDisk :: OperationType
pattern DeleteDisk = OperationType' "DeleteDisk"

pattern DeleteDiskSnapshot :: OperationType
pattern DeleteDiskSnapshot = OperationType' "DeleteDiskSnapshot"

pattern DeleteDomain :: OperationType
pattern DeleteDomain = OperationType' "DeleteDomain"

pattern DeleteDomainEntry :: OperationType
pattern DeleteDomainEntry = OperationType' "DeleteDomainEntry"

pattern DeleteInstance :: OperationType
pattern DeleteInstance = OperationType' "DeleteInstance"

pattern DeleteInstanceSnapshot :: OperationType
pattern DeleteInstanceSnapshot = OperationType' "DeleteInstanceSnapshot"

pattern DeleteKnownHostKeys :: OperationType
pattern DeleteKnownHostKeys = OperationType' "DeleteKnownHostKeys"

pattern DeleteLoadBalancer :: OperationType
pattern DeleteLoadBalancer = OperationType' "DeleteLoadBalancer"

pattern DeleteLoadBalancerTLSCertificate :: OperationType
pattern DeleteLoadBalancerTLSCertificate = OperationType' "DeleteLoadBalancerTlsCertificate"

pattern DeleteRelationalDatabase :: OperationType
pattern DeleteRelationalDatabase = OperationType' "DeleteRelationalDatabase"

pattern DeleteRelationalDatabaseSnapshot :: OperationType
pattern DeleteRelationalDatabaseSnapshot = OperationType' "DeleteRelationalDatabaseSnapshot"

pattern DetachDisk :: OperationType
pattern DetachDisk = OperationType' "DetachDisk"

pattern DetachInstancesFromLoadBalancer :: OperationType
pattern DetachInstancesFromLoadBalancer = OperationType' "DetachInstancesFromLoadBalancer"

pattern DetachStaticIP :: OperationType
pattern DetachStaticIP = OperationType' "DetachStaticIp"

pattern DisableAddOn :: OperationType
pattern DisableAddOn = OperationType' "DisableAddOn"

pattern EnableAddOn :: OperationType
pattern EnableAddOn = OperationType' "EnableAddOn"

pattern GetAlarms :: OperationType
pattern GetAlarms = OperationType' "GetAlarms"

pattern GetContactMethods :: OperationType
pattern GetContactMethods = OperationType' "GetContactMethods"

pattern OpenInstancePublicPorts :: OperationType
pattern OpenInstancePublicPorts = OperationType' "OpenInstancePublicPorts"

pattern PutAlarm :: OperationType
pattern PutAlarm = OperationType' "PutAlarm"

pattern PutInstancePublicPorts :: OperationType
pattern PutInstancePublicPorts = OperationType' "PutInstancePublicPorts"

pattern RebootInstance :: OperationType
pattern RebootInstance = OperationType' "RebootInstance"

pattern RebootRelationalDatabase :: OperationType
pattern RebootRelationalDatabase = OperationType' "RebootRelationalDatabase"

pattern ReleaseStaticIP :: OperationType
pattern ReleaseStaticIP = OperationType' "ReleaseStaticIp"

pattern SendContactMethodVerification :: OperationType
pattern SendContactMethodVerification = OperationType' "SendContactMethodVerification"

pattern StartInstance :: OperationType
pattern StartInstance = OperationType' "StartInstance"

pattern StartRelationalDatabase :: OperationType
pattern StartRelationalDatabase = OperationType' "StartRelationalDatabase"

pattern StopInstance :: OperationType
pattern StopInstance = OperationType' "StopInstance"

pattern StopRelationalDatabase :: OperationType
pattern StopRelationalDatabase = OperationType' "StopRelationalDatabase"

pattern TestAlarm :: OperationType
pattern TestAlarm = OperationType' "TestAlarm"

pattern UpdateDomainEntry :: OperationType
pattern UpdateDomainEntry = OperationType' "UpdateDomainEntry"

pattern UpdateLoadBalancerAttribute :: OperationType
pattern UpdateLoadBalancerAttribute = OperationType' "UpdateLoadBalancerAttribute"

pattern UpdateRelationalDatabase :: OperationType
pattern UpdateRelationalDatabase = OperationType' "UpdateRelationalDatabase"

pattern UpdateRelationalDatabaseParameters :: OperationType
pattern UpdateRelationalDatabaseParameters = OperationType' "UpdateRelationalDatabaseParameters"

{-# COMPLETE
  AllocateStaticIP,
  AttachDisk,
  AttachInstancesToLoadBalancer,
  AttachLoadBalancerTLSCertificate,
  AttachStaticIP,
  CloseInstancePublicPorts,
  CreateContactMethod,
  CreateDisk,
  CreateDiskFromSnapshot,
  CreateDiskSnapshot,
  CreateDomain,
  CreateInstance,
  CreateInstanceSnapshot,
  CreateInstancesFromSnapshot,
  CreateLoadBalancer,
  CreateLoadBalancerTLSCertificate,
  CreateRelationalDatabase,
  CreateRelationalDatabaseFromSnapshot,
  CreateRelationalDatabaseSnapshot,
  DeleteAlarm,
  DeleteContactMethod,
  DeleteDisk,
  DeleteDiskSnapshot,
  DeleteDomain,
  DeleteDomainEntry,
  DeleteInstance,
  DeleteInstanceSnapshot,
  DeleteKnownHostKeys,
  DeleteLoadBalancer,
  DeleteLoadBalancerTLSCertificate,
  DeleteRelationalDatabase,
  DeleteRelationalDatabaseSnapshot,
  DetachDisk,
  DetachInstancesFromLoadBalancer,
  DetachStaticIP,
  DisableAddOn,
  EnableAddOn,
  GetAlarms,
  GetContactMethods,
  OpenInstancePublicPorts,
  PutAlarm,
  PutInstancePublicPorts,
  RebootInstance,
  RebootRelationalDatabase,
  ReleaseStaticIP,
  SendContactMethodVerification,
  StartInstance,
  StartRelationalDatabase,
  StopInstance,
  StopRelationalDatabase,
  TestAlarm,
  UpdateDomainEntry,
  UpdateLoadBalancerAttribute,
  UpdateRelationalDatabase,
  UpdateRelationalDatabaseParameters,
  OperationType' #-}

instance FromText OperationType where
    parser = (OperationType' . mk) <$> takeText

instance ToText OperationType where
    toText (OperationType' ci) = original ci

-- | Represents an enum of /known/ $OperationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OperationType where
    toEnum i = case i of
        0 -> AllocateStaticIP
        1 -> AttachDisk
        2 -> AttachInstancesToLoadBalancer
        3 -> AttachLoadBalancerTLSCertificate
        4 -> AttachStaticIP
        5 -> CloseInstancePublicPorts
        6 -> CreateContactMethod
        7 -> CreateDisk
        8 -> CreateDiskFromSnapshot
        9 -> CreateDiskSnapshot
        10 -> CreateDomain
        11 -> CreateInstance
        12 -> CreateInstanceSnapshot
        13 -> CreateInstancesFromSnapshot
        14 -> CreateLoadBalancer
        15 -> CreateLoadBalancerTLSCertificate
        16 -> CreateRelationalDatabase
        17 -> CreateRelationalDatabaseFromSnapshot
        18 -> CreateRelationalDatabaseSnapshot
        19 -> DeleteAlarm
        20 -> DeleteContactMethod
        21 -> DeleteDisk
        22 -> DeleteDiskSnapshot
        23 -> DeleteDomain
        24 -> DeleteDomainEntry
        25 -> DeleteInstance
        26 -> DeleteInstanceSnapshot
        27 -> DeleteKnownHostKeys
        28 -> DeleteLoadBalancer
        29 -> DeleteLoadBalancerTLSCertificate
        30 -> DeleteRelationalDatabase
        31 -> DeleteRelationalDatabaseSnapshot
        32 -> DetachDisk
        33 -> DetachInstancesFromLoadBalancer
        34 -> DetachStaticIP
        35 -> DisableAddOn
        36 -> EnableAddOn
        37 -> GetAlarms
        38 -> GetContactMethods
        39 -> OpenInstancePublicPorts
        40 -> PutAlarm
        41 -> PutInstancePublicPorts
        42 -> RebootInstance
        43 -> RebootRelationalDatabase
        44 -> ReleaseStaticIP
        45 -> SendContactMethodVerification
        46 -> StartInstance
        47 -> StartRelationalDatabase
        48 -> StopInstance
        49 -> StopRelationalDatabase
        50 -> TestAlarm
        51 -> UpdateDomainEntry
        52 -> UpdateLoadBalancerAttribute
        53 -> UpdateRelationalDatabase
        54 -> UpdateRelationalDatabaseParameters
        _ -> (error . showText) $ "Unknown index for OperationType: " <> toText i
    fromEnum x = case x of
        AllocateStaticIP -> 0
        AttachDisk -> 1
        AttachInstancesToLoadBalancer -> 2
        AttachLoadBalancerTLSCertificate -> 3
        AttachStaticIP -> 4
        CloseInstancePublicPorts -> 5
        CreateContactMethod -> 6
        CreateDisk -> 7
        CreateDiskFromSnapshot -> 8
        CreateDiskSnapshot -> 9
        CreateDomain -> 10
        CreateInstance -> 11
        CreateInstanceSnapshot -> 12
        CreateInstancesFromSnapshot -> 13
        CreateLoadBalancer -> 14
        CreateLoadBalancerTLSCertificate -> 15
        CreateRelationalDatabase -> 16
        CreateRelationalDatabaseFromSnapshot -> 17
        CreateRelationalDatabaseSnapshot -> 18
        DeleteAlarm -> 19
        DeleteContactMethod -> 20
        DeleteDisk -> 21
        DeleteDiskSnapshot -> 22
        DeleteDomain -> 23
        DeleteDomainEntry -> 24
        DeleteInstance -> 25
        DeleteInstanceSnapshot -> 26
        DeleteKnownHostKeys -> 27
        DeleteLoadBalancer -> 28
        DeleteLoadBalancerTLSCertificate -> 29
        DeleteRelationalDatabase -> 30
        DeleteRelationalDatabaseSnapshot -> 31
        DetachDisk -> 32
        DetachInstancesFromLoadBalancer -> 33
        DetachStaticIP -> 34
        DisableAddOn -> 35
        EnableAddOn -> 36
        GetAlarms -> 37
        GetContactMethods -> 38
        OpenInstancePublicPorts -> 39
        PutAlarm -> 40
        PutInstancePublicPorts -> 41
        RebootInstance -> 42
        RebootRelationalDatabase -> 43
        ReleaseStaticIP -> 44
        SendContactMethodVerification -> 45
        StartInstance -> 46
        StartRelationalDatabase -> 47
        StopInstance -> 48
        StopRelationalDatabase -> 49
        TestAlarm -> 50
        UpdateDomainEntry -> 51
        UpdateLoadBalancerAttribute -> 52
        UpdateRelationalDatabase -> 53
        UpdateRelationalDatabaseParameters -> 54
        OperationType' name -> (error . showText) $ "Unknown OperationType: " <> original name

-- | Represents the bounds of /known/ $OperationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OperationType where
    minBound = AllocateStaticIP
    maxBound = UpdateRelationalDatabaseParameters

instance Hashable     OperationType
instance NFData       OperationType
instance ToByteString OperationType
instance ToQuery      OperationType
instance ToHeader     OperationType

instance FromJSON OperationType where
    parseJSON = parseJSONText "OperationType"
