{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CapacityReservationInstancePlatform
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.CapacityReservationInstancePlatform (
  CapacityReservationInstancePlatform (
    ..
    , CRIPLinuxUnix
    , CRIPLinuxWithSqlServerEnterprise
    , CRIPLinuxWithSqlServerStandard
    , CRIPLinuxWithSqlServerWeb
    , CRIPRedHatEnterpriseLinux
    , CRIPSuseLinux
    , CRIPWindows
    , CRIPWindowsWithSqlServer
    , CRIPWindowsWithSqlServerEnterprise
    , CRIPWindowsWithSqlServerStandard
    , CRIPWindowsWithSqlServerWeb
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data CapacityReservationInstancePlatform = CapacityReservationInstancePlatform' (CI
                                                                                   Text)
                                             deriving (Eq, Ord, Read, Show,
                                                       Data, Typeable, Generic)

pattern CRIPLinuxUnix :: CapacityReservationInstancePlatform
pattern CRIPLinuxUnix = CapacityReservationInstancePlatform' "Linux/UNIX"

pattern CRIPLinuxWithSqlServerEnterprise :: CapacityReservationInstancePlatform
pattern CRIPLinuxWithSqlServerEnterprise = CapacityReservationInstancePlatform' "Linux with SQL Server Enterprise"

pattern CRIPLinuxWithSqlServerStandard :: CapacityReservationInstancePlatform
pattern CRIPLinuxWithSqlServerStandard = CapacityReservationInstancePlatform' "Linux with SQL Server Standard"

pattern CRIPLinuxWithSqlServerWeb :: CapacityReservationInstancePlatform
pattern CRIPLinuxWithSqlServerWeb = CapacityReservationInstancePlatform' "Linux with SQL Server Web"

pattern CRIPRedHatEnterpriseLinux :: CapacityReservationInstancePlatform
pattern CRIPRedHatEnterpriseLinux = CapacityReservationInstancePlatform' "Red Hat Enterprise Linux"

pattern CRIPSuseLinux :: CapacityReservationInstancePlatform
pattern CRIPSuseLinux = CapacityReservationInstancePlatform' "SUSE Linux"

pattern CRIPWindows :: CapacityReservationInstancePlatform
pattern CRIPWindows = CapacityReservationInstancePlatform' "Windows"

pattern CRIPWindowsWithSqlServer :: CapacityReservationInstancePlatform
pattern CRIPWindowsWithSqlServer = CapacityReservationInstancePlatform' "Windows with SQL Server"

pattern CRIPWindowsWithSqlServerEnterprise :: CapacityReservationInstancePlatform
pattern CRIPWindowsWithSqlServerEnterprise = CapacityReservationInstancePlatform' "Windows with SQL Server Enterprise"

pattern CRIPWindowsWithSqlServerStandard :: CapacityReservationInstancePlatform
pattern CRIPWindowsWithSqlServerStandard = CapacityReservationInstancePlatform' "Windows with SQL Server Standard"

pattern CRIPWindowsWithSqlServerWeb :: CapacityReservationInstancePlatform
pattern CRIPWindowsWithSqlServerWeb = CapacityReservationInstancePlatform' "Windows with SQL Server Web"

{-# COMPLETE
  CRIPLinuxUnix,
  CRIPLinuxWithSqlServerEnterprise,
  CRIPLinuxWithSqlServerStandard,
  CRIPLinuxWithSqlServerWeb,
  CRIPRedHatEnterpriseLinux,
  CRIPSuseLinux,
  CRIPWindows,
  CRIPWindowsWithSqlServer,
  CRIPWindowsWithSqlServerEnterprise,
  CRIPWindowsWithSqlServerStandard,
  CRIPWindowsWithSqlServerWeb,
  CapacityReservationInstancePlatform' #-}

instance FromText CapacityReservationInstancePlatform where
    parser = (CapacityReservationInstancePlatform' . mk) <$> takeText

instance ToText CapacityReservationInstancePlatform where
    toText (CapacityReservationInstancePlatform' ci) = original ci

-- | Represents an enum of /known/ $CapacityReservationInstancePlatform.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CapacityReservationInstancePlatform where
    toEnum i = case i of
        0 -> CRIPLinuxUnix
        1 -> CRIPLinuxWithSqlServerEnterprise
        2 -> CRIPLinuxWithSqlServerStandard
        3 -> CRIPLinuxWithSqlServerWeb
        4 -> CRIPRedHatEnterpriseLinux
        5 -> CRIPSuseLinux
        6 -> CRIPWindows
        7 -> CRIPWindowsWithSqlServer
        8 -> CRIPWindowsWithSqlServerEnterprise
        9 -> CRIPWindowsWithSqlServerStandard
        10 -> CRIPWindowsWithSqlServerWeb
        _ -> (error . showText) $ "Unknown index for CapacityReservationInstancePlatform: " <> toText i
    fromEnum x = case x of
        CRIPLinuxUnix -> 0
        CRIPLinuxWithSqlServerEnterprise -> 1
        CRIPLinuxWithSqlServerStandard -> 2
        CRIPLinuxWithSqlServerWeb -> 3
        CRIPRedHatEnterpriseLinux -> 4
        CRIPSuseLinux -> 5
        CRIPWindows -> 6
        CRIPWindowsWithSqlServer -> 7
        CRIPWindowsWithSqlServerEnterprise -> 8
        CRIPWindowsWithSqlServerStandard -> 9
        CRIPWindowsWithSqlServerWeb -> 10
        CapacityReservationInstancePlatform' name -> (error . showText) $ "Unknown CapacityReservationInstancePlatform: " <> original name

-- | Represents the bounds of /known/ $CapacityReservationInstancePlatform.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CapacityReservationInstancePlatform where
    minBound = CRIPLinuxUnix
    maxBound = CRIPWindowsWithSqlServerWeb

instance Hashable     CapacityReservationInstancePlatform
instance NFData       CapacityReservationInstancePlatform
instance ToByteString CapacityReservationInstancePlatform
instance ToQuery      CapacityReservationInstancePlatform
instance ToHeader     CapacityReservationInstancePlatform

instance FromXML CapacityReservationInstancePlatform where
    parseXML = parseXMLText "CapacityReservationInstancePlatform"
