{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InstanceType (
  InstanceType (
    ..
    , A1_2XLarge
    , A1_4XLarge
    , A1_Large
    , A1_Medium
    , A1_Metal
    , A1_XLarge
    , C1_Medium
    , C1_XLarge
    , C3_2XLarge
    , C3_4XLarge
    , C3_8XLarge
    , C3_Large
    , C3_XLarge
    , C4_2XLarge
    , C4_4XLarge
    , C4_8XLarge
    , C4_Large
    , C4_XLarge
    , C5_12XLarge
    , C5_18XLarge
    , C5_24XLarge
    , C5_2XLarge
    , C5_4XLarge
    , C5_9XLarge
    , C5_Large
    , C5_Metal
    , C5_XLarge
    , C5d_12XLarge
    , C5d_18XLarge
    , C5d_24XLarge
    , C5d_2XLarge
    , C5d_4XLarge
    , C5d_9XLarge
    , C5d_Large
    , C5d_Metal
    , C5d_XLarge
    , C5n_18XLarge
    , C5n_2XLarge
    , C5n_4XLarge
    , C5n_9XLarge
    , C5n_Large
    , C5n_XLarge
    , CC1_4XLarge
    , CC2_8XLarge
    , CG1_4XLarge
    , CR1_8XLarge
    , D2_2XLarge
    , D2_4XLarge
    , D2_8XLarge
    , D2_XLarge
    , F1_16XLarge
    , F1_2XLarge
    , F1_4XLarge
    , G2_2XLarge
    , G2_8XLarge
    , G3_16XLarge
    , G3_4XLarge
    , G3_8XLarge
    , G3s_XLarge
    , G4dn_12XLarge
    , G4dn_16XLarge
    , G4dn_2XLarge
    , G4dn_4XLarge
    , G4dn_8XLarge
    , G4dn_XLarge
    , H1_16XLarge
    , H1_2XLarge
    , H1_4XLarge
    , H1_8XLarge
    , HI1_4XLarge
    , HS1_8XLarge
    , I2_2XLarge
    , I2_4XLarge
    , I2_8XLarge
    , I2_XLarge
    , I3_16XLarge
    , I3_2XLarge
    , I3_4XLarge
    , I3_8XLarge
    , I3_Large
    , I3_Metal
    , I3_XLarge
    , I3en_12XLarge
    , I3en_24XLarge
    , I3en_2XLarge
    , I3en_3XLarge
    , I3en_6XLarge
    , I3en_Large
    , I3en_Metal
    , I3en_XLarge
    , INF1_24XLarge
    , INF1_2XLarge
    , INF1_6XLarge
    , INF1_XLarge
    , M1_Large
    , M1_Medium
    , M1_Small
    , M1_XLarge
    , M2_2XLarge
    , M2_4XLarge
    , M2_XLarge
    , M3_2XLarge
    , M3_Large
    , M3_Medium
    , M3_XLarge
    , M4_10XLarge
    , M4_16XLarge
    , M4_2XLarge
    , M4_4XLarge
    , M4_Large
    , M4_XLarge
    , M5_12XLarge
    , M5_16XLarge
    , M5_24XLarge
    , M5_2XLarge
    , M5_4XLarge
    , M5_8XLarge
    , M5_Large
    , M5_Metal
    , M5_XLarge
    , M5a_12XLarge
    , M5a_16XLarge
    , M5a_24XLarge
    , M5a_2XLarge
    , M5a_4XLarge
    , M5a_8XLarge
    , M5a_Large
    , M5a_XLarge
    , M5ad_12XLarge
    , M5ad_16XLarge
    , M5ad_24XLarge
    , M5ad_2XLarge
    , M5ad_4XLarge
    , M5ad_8XLarge
    , M5ad_Large
    , M5ad_XLarge
    , M5d_12XLarge
    , M5d_16XLarge
    , M5d_24XLarge
    , M5d_2XLarge
    , M5d_4XLarge
    , M5d_8XLarge
    , M5d_Large
    , M5d_Metal
    , M5d_XLarge
    , M5dn_12XLarge
    , M5dn_16XLarge
    , M5dn_24XLarge
    , M5dn_2XLarge
    , M5dn_4XLarge
    , M5dn_8XLarge
    , M5dn_Large
    , M5dn_XLarge
    , M5n_12XLarge
    , M5n_16XLarge
    , M5n_24XLarge
    , M5n_2XLarge
    , M5n_4XLarge
    , M5n_8XLarge
    , M5n_Large
    , M5n_XLarge
    , P2_16XLarge
    , P2_8XLarge
    , P2_XLarge
    , P3_16XLarge
    , P3_2XLarge
    , P3_8XLarge
    , P3dn_24XLarge
    , R3_2XLarge
    , R3_4XLarge
    , R3_8XLarge
    , R3_Large
    , R3_XLarge
    , R4_16XLarge
    , R4_2XLarge
    , R4_4XLarge
    , R4_8XLarge
    , R4_Large
    , R4_XLarge
    , R5_12XLarge
    , R5_16XLarge
    , R5_24XLarge
    , R5_2XLarge
    , R5_4XLarge
    , R5_8XLarge
    , R5_Large
    , R5_Metal
    , R5_XLarge
    , R5a_12XLarge
    , R5a_16XLarge
    , R5a_24XLarge
    , R5a_2XLarge
    , R5a_4XLarge
    , R5a_8XLarge
    , R5a_Large
    , R5a_XLarge
    , R5ad_12XLarge
    , R5ad_16XLarge
    , R5ad_24XLarge
    , R5ad_2XLarge
    , R5ad_4XLarge
    , R5ad_8XLarge
    , R5ad_Large
    , R5ad_XLarge
    , R5d_12XLarge
    , R5d_16XLarge
    , R5d_24XLarge
    , R5d_2XLarge
    , R5d_4XLarge
    , R5d_8XLarge
    , R5d_Large
    , R5d_Metal
    , R5d_XLarge
    , R5dn_12XLarge
    , R5dn_16XLarge
    , R5dn_24XLarge
    , R5dn_2XLarge
    , R5dn_4XLarge
    , R5dn_8XLarge
    , R5dn_Large
    , R5dn_XLarge
    , R5n_12XLarge
    , R5n_16XLarge
    , R5n_24XLarge
    , R5n_2XLarge
    , R5n_4XLarge
    , R5n_8XLarge
    , R5n_Large
    , R5n_XLarge
    , T1_Micro
    , T2_2XLarge
    , T2_Large
    , T2_Medium
    , T2_Micro
    , T2_Nano
    , T2_Small
    , T2_XLarge
    , T3_2XLarge
    , T3_Large
    , T3_Medium
    , T3_Micro
    , T3_Nano
    , T3_Small
    , T3_XLarge
    , T3a_2XLarge
    , T3a_Large
    , T3a_Medium
    , T3a_Micro
    , T3a_Nano
    , T3a_Small
    , T3a_XLarge
    , U12TB1_Metal
    , U18TB1_Metal
    , U24TB1_Metal
    , U6TB1_Metal
    , U9TB1_Metal
    , X1_16XLarge
    , X1_32XLarge
    , X1e_16XLarge
    , X1e_2XLarge
    , X1e_32XLarge
    , X1e_4XLarge
    , X1e_8XLarge
    , X1e_XLarge
    , Z1d_12XLarge
    , Z1d_2XLarge
    , Z1d_3XLarge
    , Z1d_6XLarge
    , Z1d_Large
    , Z1d_Metal
    , Z1d_XLarge
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data InstanceType = InstanceType' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern A1_2XLarge :: InstanceType
pattern A1_2XLarge = InstanceType' "a1.2xlarge"

pattern A1_4XLarge :: InstanceType
pattern A1_4XLarge = InstanceType' "a1.4xlarge"

pattern A1_Large :: InstanceType
pattern A1_Large = InstanceType' "a1.large"

pattern A1_Medium :: InstanceType
pattern A1_Medium = InstanceType' "a1.medium"

pattern A1_Metal :: InstanceType
pattern A1_Metal = InstanceType' "a1.metal"

pattern A1_XLarge :: InstanceType
pattern A1_XLarge = InstanceType' "a1.xlarge"

pattern C1_Medium :: InstanceType
pattern C1_Medium = InstanceType' "c1.medium"

pattern C1_XLarge :: InstanceType
pattern C1_XLarge = InstanceType' "c1.xlarge"

pattern C3_2XLarge :: InstanceType
pattern C3_2XLarge = InstanceType' "c3.2xlarge"

pattern C3_4XLarge :: InstanceType
pattern C3_4XLarge = InstanceType' "c3.4xlarge"

pattern C3_8XLarge :: InstanceType
pattern C3_8XLarge = InstanceType' "c3.8xlarge"

pattern C3_Large :: InstanceType
pattern C3_Large = InstanceType' "c3.large"

pattern C3_XLarge :: InstanceType
pattern C3_XLarge = InstanceType' "c3.xlarge"

pattern C4_2XLarge :: InstanceType
pattern C4_2XLarge = InstanceType' "c4.2xlarge"

pattern C4_4XLarge :: InstanceType
pattern C4_4XLarge = InstanceType' "c4.4xlarge"

pattern C4_8XLarge :: InstanceType
pattern C4_8XLarge = InstanceType' "c4.8xlarge"

pattern C4_Large :: InstanceType
pattern C4_Large = InstanceType' "c4.large"

pattern C4_XLarge :: InstanceType
pattern C4_XLarge = InstanceType' "c4.xlarge"

pattern C5_12XLarge :: InstanceType
pattern C5_12XLarge = InstanceType' "c5.12xlarge"

pattern C5_18XLarge :: InstanceType
pattern C5_18XLarge = InstanceType' "c5.18xlarge"

pattern C5_24XLarge :: InstanceType
pattern C5_24XLarge = InstanceType' "c5.24xlarge"

pattern C5_2XLarge :: InstanceType
pattern C5_2XLarge = InstanceType' "c5.2xlarge"

pattern C5_4XLarge :: InstanceType
pattern C5_4XLarge = InstanceType' "c5.4xlarge"

pattern C5_9XLarge :: InstanceType
pattern C5_9XLarge = InstanceType' "c5.9xlarge"

pattern C5_Large :: InstanceType
pattern C5_Large = InstanceType' "c5.large"

pattern C5_Metal :: InstanceType
pattern C5_Metal = InstanceType' "c5.metal"

pattern C5_XLarge :: InstanceType
pattern C5_XLarge = InstanceType' "c5.xlarge"

pattern C5d_12XLarge :: InstanceType
pattern C5d_12XLarge = InstanceType' "c5d.12xlarge"

pattern C5d_18XLarge :: InstanceType
pattern C5d_18XLarge = InstanceType' "c5d.18xlarge"

pattern C5d_24XLarge :: InstanceType
pattern C5d_24XLarge = InstanceType' "c5d.24xlarge"

pattern C5d_2XLarge :: InstanceType
pattern C5d_2XLarge = InstanceType' "c5d.2xlarge"

pattern C5d_4XLarge :: InstanceType
pattern C5d_4XLarge = InstanceType' "c5d.4xlarge"

pattern C5d_9XLarge :: InstanceType
pattern C5d_9XLarge = InstanceType' "c5d.9xlarge"

pattern C5d_Large :: InstanceType
pattern C5d_Large = InstanceType' "c5d.large"

pattern C5d_Metal :: InstanceType
pattern C5d_Metal = InstanceType' "c5d.metal"

pattern C5d_XLarge :: InstanceType
pattern C5d_XLarge = InstanceType' "c5d.xlarge"

pattern C5n_18XLarge :: InstanceType
pattern C5n_18XLarge = InstanceType' "c5n.18xlarge"

pattern C5n_2XLarge :: InstanceType
pattern C5n_2XLarge = InstanceType' "c5n.2xlarge"

pattern C5n_4XLarge :: InstanceType
pattern C5n_4XLarge = InstanceType' "c5n.4xlarge"

pattern C5n_9XLarge :: InstanceType
pattern C5n_9XLarge = InstanceType' "c5n.9xlarge"

pattern C5n_Large :: InstanceType
pattern C5n_Large = InstanceType' "c5n.large"

pattern C5n_XLarge :: InstanceType
pattern C5n_XLarge = InstanceType' "c5n.xlarge"

pattern CC1_4XLarge :: InstanceType
pattern CC1_4XLarge = InstanceType' "cc1.4xlarge"

pattern CC2_8XLarge :: InstanceType
pattern CC2_8XLarge = InstanceType' "cc2.8xlarge"

pattern CG1_4XLarge :: InstanceType
pattern CG1_4XLarge = InstanceType' "cg1.4xlarge"

pattern CR1_8XLarge :: InstanceType
pattern CR1_8XLarge = InstanceType' "cr1.8xlarge"

pattern D2_2XLarge :: InstanceType
pattern D2_2XLarge = InstanceType' "d2.2xlarge"

pattern D2_4XLarge :: InstanceType
pattern D2_4XLarge = InstanceType' "d2.4xlarge"

pattern D2_8XLarge :: InstanceType
pattern D2_8XLarge = InstanceType' "d2.8xlarge"

pattern D2_XLarge :: InstanceType
pattern D2_XLarge = InstanceType' "d2.xlarge"

pattern F1_16XLarge :: InstanceType
pattern F1_16XLarge = InstanceType' "f1.16xlarge"

pattern F1_2XLarge :: InstanceType
pattern F1_2XLarge = InstanceType' "f1.2xlarge"

pattern F1_4XLarge :: InstanceType
pattern F1_4XLarge = InstanceType' "f1.4xlarge"

pattern G2_2XLarge :: InstanceType
pattern G2_2XLarge = InstanceType' "g2.2xlarge"

pattern G2_8XLarge :: InstanceType
pattern G2_8XLarge = InstanceType' "g2.8xlarge"

pattern G3_16XLarge :: InstanceType
pattern G3_16XLarge = InstanceType' "g3.16xlarge"

pattern G3_4XLarge :: InstanceType
pattern G3_4XLarge = InstanceType' "g3.4xlarge"

pattern G3_8XLarge :: InstanceType
pattern G3_8XLarge = InstanceType' "g3.8xlarge"

pattern G3s_XLarge :: InstanceType
pattern G3s_XLarge = InstanceType' "g3s.xlarge"

pattern G4dn_12XLarge :: InstanceType
pattern G4dn_12XLarge = InstanceType' "g4dn.12xlarge"

pattern G4dn_16XLarge :: InstanceType
pattern G4dn_16XLarge = InstanceType' "g4dn.16xlarge"

pattern G4dn_2XLarge :: InstanceType
pattern G4dn_2XLarge = InstanceType' "g4dn.2xlarge"

pattern G4dn_4XLarge :: InstanceType
pattern G4dn_4XLarge = InstanceType' "g4dn.4xlarge"

pattern G4dn_8XLarge :: InstanceType
pattern G4dn_8XLarge = InstanceType' "g4dn.8xlarge"

pattern G4dn_XLarge :: InstanceType
pattern G4dn_XLarge = InstanceType' "g4dn.xlarge"

pattern H1_16XLarge :: InstanceType
pattern H1_16XLarge = InstanceType' "h1.16xlarge"

pattern H1_2XLarge :: InstanceType
pattern H1_2XLarge = InstanceType' "h1.2xlarge"

pattern H1_4XLarge :: InstanceType
pattern H1_4XLarge = InstanceType' "h1.4xlarge"

pattern H1_8XLarge :: InstanceType
pattern H1_8XLarge = InstanceType' "h1.8xlarge"

pattern HI1_4XLarge :: InstanceType
pattern HI1_4XLarge = InstanceType' "hi1.4xlarge"

pattern HS1_8XLarge :: InstanceType
pattern HS1_8XLarge = InstanceType' "hs1.8xlarge"

pattern I2_2XLarge :: InstanceType
pattern I2_2XLarge = InstanceType' "i2.2xlarge"

pattern I2_4XLarge :: InstanceType
pattern I2_4XLarge = InstanceType' "i2.4xlarge"

pattern I2_8XLarge :: InstanceType
pattern I2_8XLarge = InstanceType' "i2.8xlarge"

pattern I2_XLarge :: InstanceType
pattern I2_XLarge = InstanceType' "i2.xlarge"

pattern I3_16XLarge :: InstanceType
pattern I3_16XLarge = InstanceType' "i3.16xlarge"

pattern I3_2XLarge :: InstanceType
pattern I3_2XLarge = InstanceType' "i3.2xlarge"

pattern I3_4XLarge :: InstanceType
pattern I3_4XLarge = InstanceType' "i3.4xlarge"

pattern I3_8XLarge :: InstanceType
pattern I3_8XLarge = InstanceType' "i3.8xlarge"

pattern I3_Large :: InstanceType
pattern I3_Large = InstanceType' "i3.large"

pattern I3_Metal :: InstanceType
pattern I3_Metal = InstanceType' "i3.metal"

pattern I3_XLarge :: InstanceType
pattern I3_XLarge = InstanceType' "i3.xlarge"

pattern I3en_12XLarge :: InstanceType
pattern I3en_12XLarge = InstanceType' "i3en.12xlarge"

pattern I3en_24XLarge :: InstanceType
pattern I3en_24XLarge = InstanceType' "i3en.24xlarge"

pattern I3en_2XLarge :: InstanceType
pattern I3en_2XLarge = InstanceType' "i3en.2xlarge"

pattern I3en_3XLarge :: InstanceType
pattern I3en_3XLarge = InstanceType' "i3en.3xlarge"

pattern I3en_6XLarge :: InstanceType
pattern I3en_6XLarge = InstanceType' "i3en.6xlarge"

pattern I3en_Large :: InstanceType
pattern I3en_Large = InstanceType' "i3en.large"

pattern I3en_Metal :: InstanceType
pattern I3en_Metal = InstanceType' "i3en.metal"

pattern I3en_XLarge :: InstanceType
pattern I3en_XLarge = InstanceType' "i3en.xlarge"

pattern INF1_24XLarge :: InstanceType
pattern INF1_24XLarge = InstanceType' "inf1.24xlarge"

pattern INF1_2XLarge :: InstanceType
pattern INF1_2XLarge = InstanceType' "inf1.2xlarge"

pattern INF1_6XLarge :: InstanceType
pattern INF1_6XLarge = InstanceType' "inf1.6xlarge"

pattern INF1_XLarge :: InstanceType
pattern INF1_XLarge = InstanceType' "inf1.xlarge"

pattern M1_Large :: InstanceType
pattern M1_Large = InstanceType' "m1.large"

pattern M1_Medium :: InstanceType
pattern M1_Medium = InstanceType' "m1.medium"

pattern M1_Small :: InstanceType
pattern M1_Small = InstanceType' "m1.small"

pattern M1_XLarge :: InstanceType
pattern M1_XLarge = InstanceType' "m1.xlarge"

pattern M2_2XLarge :: InstanceType
pattern M2_2XLarge = InstanceType' "m2.2xlarge"

pattern M2_4XLarge :: InstanceType
pattern M2_4XLarge = InstanceType' "m2.4xlarge"

pattern M2_XLarge :: InstanceType
pattern M2_XLarge = InstanceType' "m2.xlarge"

pattern M3_2XLarge :: InstanceType
pattern M3_2XLarge = InstanceType' "m3.2xlarge"

pattern M3_Large :: InstanceType
pattern M3_Large = InstanceType' "m3.large"

pattern M3_Medium :: InstanceType
pattern M3_Medium = InstanceType' "m3.medium"

pattern M3_XLarge :: InstanceType
pattern M3_XLarge = InstanceType' "m3.xlarge"

pattern M4_10XLarge :: InstanceType
pattern M4_10XLarge = InstanceType' "m4.10xlarge"

pattern M4_16XLarge :: InstanceType
pattern M4_16XLarge = InstanceType' "m4.16xlarge"

pattern M4_2XLarge :: InstanceType
pattern M4_2XLarge = InstanceType' "m4.2xlarge"

pattern M4_4XLarge :: InstanceType
pattern M4_4XLarge = InstanceType' "m4.4xlarge"

pattern M4_Large :: InstanceType
pattern M4_Large = InstanceType' "m4.large"

pattern M4_XLarge :: InstanceType
pattern M4_XLarge = InstanceType' "m4.xlarge"

pattern M5_12XLarge :: InstanceType
pattern M5_12XLarge = InstanceType' "m5.12xlarge"

pattern M5_16XLarge :: InstanceType
pattern M5_16XLarge = InstanceType' "m5.16xlarge"

pattern M5_24XLarge :: InstanceType
pattern M5_24XLarge = InstanceType' "m5.24xlarge"

pattern M5_2XLarge :: InstanceType
pattern M5_2XLarge = InstanceType' "m5.2xlarge"

pattern M5_4XLarge :: InstanceType
pattern M5_4XLarge = InstanceType' "m5.4xlarge"

pattern M5_8XLarge :: InstanceType
pattern M5_8XLarge = InstanceType' "m5.8xlarge"

pattern M5_Large :: InstanceType
pattern M5_Large = InstanceType' "m5.large"

pattern M5_Metal :: InstanceType
pattern M5_Metal = InstanceType' "m5.metal"

pattern M5_XLarge :: InstanceType
pattern M5_XLarge = InstanceType' "m5.xlarge"

pattern M5a_12XLarge :: InstanceType
pattern M5a_12XLarge = InstanceType' "m5a.12xlarge"

pattern M5a_16XLarge :: InstanceType
pattern M5a_16XLarge = InstanceType' "m5a.16xlarge"

pattern M5a_24XLarge :: InstanceType
pattern M5a_24XLarge = InstanceType' "m5a.24xlarge"

pattern M5a_2XLarge :: InstanceType
pattern M5a_2XLarge = InstanceType' "m5a.2xlarge"

pattern M5a_4XLarge :: InstanceType
pattern M5a_4XLarge = InstanceType' "m5a.4xlarge"

pattern M5a_8XLarge :: InstanceType
pattern M5a_8XLarge = InstanceType' "m5a.8xlarge"

pattern M5a_Large :: InstanceType
pattern M5a_Large = InstanceType' "m5a.large"

pattern M5a_XLarge :: InstanceType
pattern M5a_XLarge = InstanceType' "m5a.xlarge"

pattern M5ad_12XLarge :: InstanceType
pattern M5ad_12XLarge = InstanceType' "m5ad.12xlarge"

pattern M5ad_16XLarge :: InstanceType
pattern M5ad_16XLarge = InstanceType' "m5ad.16xlarge"

pattern M5ad_24XLarge :: InstanceType
pattern M5ad_24XLarge = InstanceType' "m5ad.24xlarge"

pattern M5ad_2XLarge :: InstanceType
pattern M5ad_2XLarge = InstanceType' "m5ad.2xlarge"

pattern M5ad_4XLarge :: InstanceType
pattern M5ad_4XLarge = InstanceType' "m5ad.4xlarge"

pattern M5ad_8XLarge :: InstanceType
pattern M5ad_8XLarge = InstanceType' "m5ad.8xlarge"

pattern M5ad_Large :: InstanceType
pattern M5ad_Large = InstanceType' "m5ad.large"

pattern M5ad_XLarge :: InstanceType
pattern M5ad_XLarge = InstanceType' "m5ad.xlarge"

pattern M5d_12XLarge :: InstanceType
pattern M5d_12XLarge = InstanceType' "m5d.12xlarge"

pattern M5d_16XLarge :: InstanceType
pattern M5d_16XLarge = InstanceType' "m5d.16xlarge"

pattern M5d_24XLarge :: InstanceType
pattern M5d_24XLarge = InstanceType' "m5d.24xlarge"

pattern M5d_2XLarge :: InstanceType
pattern M5d_2XLarge = InstanceType' "m5d.2xlarge"

pattern M5d_4XLarge :: InstanceType
pattern M5d_4XLarge = InstanceType' "m5d.4xlarge"

pattern M5d_8XLarge :: InstanceType
pattern M5d_8XLarge = InstanceType' "m5d.8xlarge"

pattern M5d_Large :: InstanceType
pattern M5d_Large = InstanceType' "m5d.large"

pattern M5d_Metal :: InstanceType
pattern M5d_Metal = InstanceType' "m5d.metal"

pattern M5d_XLarge :: InstanceType
pattern M5d_XLarge = InstanceType' "m5d.xlarge"

pattern M5dn_12XLarge :: InstanceType
pattern M5dn_12XLarge = InstanceType' "m5dn.12xlarge"

pattern M5dn_16XLarge :: InstanceType
pattern M5dn_16XLarge = InstanceType' "m5dn.16xlarge"

pattern M5dn_24XLarge :: InstanceType
pattern M5dn_24XLarge = InstanceType' "m5dn.24xlarge"

pattern M5dn_2XLarge :: InstanceType
pattern M5dn_2XLarge = InstanceType' "m5dn.2xlarge"

pattern M5dn_4XLarge :: InstanceType
pattern M5dn_4XLarge = InstanceType' "m5dn.4xlarge"

pattern M5dn_8XLarge :: InstanceType
pattern M5dn_8XLarge = InstanceType' "m5dn.8xlarge"

pattern M5dn_Large :: InstanceType
pattern M5dn_Large = InstanceType' "m5dn.large"

pattern M5dn_XLarge :: InstanceType
pattern M5dn_XLarge = InstanceType' "m5dn.xlarge"

pattern M5n_12XLarge :: InstanceType
pattern M5n_12XLarge = InstanceType' "m5n.12xlarge"

pattern M5n_16XLarge :: InstanceType
pattern M5n_16XLarge = InstanceType' "m5n.16xlarge"

pattern M5n_24XLarge :: InstanceType
pattern M5n_24XLarge = InstanceType' "m5n.24xlarge"

pattern M5n_2XLarge :: InstanceType
pattern M5n_2XLarge = InstanceType' "m5n.2xlarge"

pattern M5n_4XLarge :: InstanceType
pattern M5n_4XLarge = InstanceType' "m5n.4xlarge"

pattern M5n_8XLarge :: InstanceType
pattern M5n_8XLarge = InstanceType' "m5n.8xlarge"

pattern M5n_Large :: InstanceType
pattern M5n_Large = InstanceType' "m5n.large"

pattern M5n_XLarge :: InstanceType
pattern M5n_XLarge = InstanceType' "m5n.xlarge"

pattern P2_16XLarge :: InstanceType
pattern P2_16XLarge = InstanceType' "p2.16xlarge"

pattern P2_8XLarge :: InstanceType
pattern P2_8XLarge = InstanceType' "p2.8xlarge"

pattern P2_XLarge :: InstanceType
pattern P2_XLarge = InstanceType' "p2.xlarge"

pattern P3_16XLarge :: InstanceType
pattern P3_16XLarge = InstanceType' "p3.16xlarge"

pattern P3_2XLarge :: InstanceType
pattern P3_2XLarge = InstanceType' "p3.2xlarge"

pattern P3_8XLarge :: InstanceType
pattern P3_8XLarge = InstanceType' "p3.8xlarge"

pattern P3dn_24XLarge :: InstanceType
pattern P3dn_24XLarge = InstanceType' "p3dn.24xlarge"

pattern R3_2XLarge :: InstanceType
pattern R3_2XLarge = InstanceType' "r3.2xlarge"

pattern R3_4XLarge :: InstanceType
pattern R3_4XLarge = InstanceType' "r3.4xlarge"

pattern R3_8XLarge :: InstanceType
pattern R3_8XLarge = InstanceType' "r3.8xlarge"

pattern R3_Large :: InstanceType
pattern R3_Large = InstanceType' "r3.large"

pattern R3_XLarge :: InstanceType
pattern R3_XLarge = InstanceType' "r3.xlarge"

pattern R4_16XLarge :: InstanceType
pattern R4_16XLarge = InstanceType' "r4.16xlarge"

pattern R4_2XLarge :: InstanceType
pattern R4_2XLarge = InstanceType' "r4.2xlarge"

pattern R4_4XLarge :: InstanceType
pattern R4_4XLarge = InstanceType' "r4.4xlarge"

pattern R4_8XLarge :: InstanceType
pattern R4_8XLarge = InstanceType' "r4.8xlarge"

pattern R4_Large :: InstanceType
pattern R4_Large = InstanceType' "r4.large"

pattern R4_XLarge :: InstanceType
pattern R4_XLarge = InstanceType' "r4.xlarge"

pattern R5_12XLarge :: InstanceType
pattern R5_12XLarge = InstanceType' "r5.12xlarge"

pattern R5_16XLarge :: InstanceType
pattern R5_16XLarge = InstanceType' "r5.16xlarge"

pattern R5_24XLarge :: InstanceType
pattern R5_24XLarge = InstanceType' "r5.24xlarge"

pattern R5_2XLarge :: InstanceType
pattern R5_2XLarge = InstanceType' "r5.2xlarge"

pattern R5_4XLarge :: InstanceType
pattern R5_4XLarge = InstanceType' "r5.4xlarge"

pattern R5_8XLarge :: InstanceType
pattern R5_8XLarge = InstanceType' "r5.8xlarge"

pattern R5_Large :: InstanceType
pattern R5_Large = InstanceType' "r5.large"

pattern R5_Metal :: InstanceType
pattern R5_Metal = InstanceType' "r5.metal"

pattern R5_XLarge :: InstanceType
pattern R5_XLarge = InstanceType' "r5.xlarge"

pattern R5a_12XLarge :: InstanceType
pattern R5a_12XLarge = InstanceType' "r5a.12xlarge"

pattern R5a_16XLarge :: InstanceType
pattern R5a_16XLarge = InstanceType' "r5a.16xlarge"

pattern R5a_24XLarge :: InstanceType
pattern R5a_24XLarge = InstanceType' "r5a.24xlarge"

pattern R5a_2XLarge :: InstanceType
pattern R5a_2XLarge = InstanceType' "r5a.2xlarge"

pattern R5a_4XLarge :: InstanceType
pattern R5a_4XLarge = InstanceType' "r5a.4xlarge"

pattern R5a_8XLarge :: InstanceType
pattern R5a_8XLarge = InstanceType' "r5a.8xlarge"

pattern R5a_Large :: InstanceType
pattern R5a_Large = InstanceType' "r5a.large"

pattern R5a_XLarge :: InstanceType
pattern R5a_XLarge = InstanceType' "r5a.xlarge"

pattern R5ad_12XLarge :: InstanceType
pattern R5ad_12XLarge = InstanceType' "r5ad.12xlarge"

pattern R5ad_16XLarge :: InstanceType
pattern R5ad_16XLarge = InstanceType' "r5ad.16xlarge"

pattern R5ad_24XLarge :: InstanceType
pattern R5ad_24XLarge = InstanceType' "r5ad.24xlarge"

pattern R5ad_2XLarge :: InstanceType
pattern R5ad_2XLarge = InstanceType' "r5ad.2xlarge"

pattern R5ad_4XLarge :: InstanceType
pattern R5ad_4XLarge = InstanceType' "r5ad.4xlarge"

pattern R5ad_8XLarge :: InstanceType
pattern R5ad_8XLarge = InstanceType' "r5ad.8xlarge"

pattern R5ad_Large :: InstanceType
pattern R5ad_Large = InstanceType' "r5ad.large"

pattern R5ad_XLarge :: InstanceType
pattern R5ad_XLarge = InstanceType' "r5ad.xlarge"

pattern R5d_12XLarge :: InstanceType
pattern R5d_12XLarge = InstanceType' "r5d.12xlarge"

pattern R5d_16XLarge :: InstanceType
pattern R5d_16XLarge = InstanceType' "r5d.16xlarge"

pattern R5d_24XLarge :: InstanceType
pattern R5d_24XLarge = InstanceType' "r5d.24xlarge"

pattern R5d_2XLarge :: InstanceType
pattern R5d_2XLarge = InstanceType' "r5d.2xlarge"

pattern R5d_4XLarge :: InstanceType
pattern R5d_4XLarge = InstanceType' "r5d.4xlarge"

pattern R5d_8XLarge :: InstanceType
pattern R5d_8XLarge = InstanceType' "r5d.8xlarge"

pattern R5d_Large :: InstanceType
pattern R5d_Large = InstanceType' "r5d.large"

pattern R5d_Metal :: InstanceType
pattern R5d_Metal = InstanceType' "r5d.metal"

pattern R5d_XLarge :: InstanceType
pattern R5d_XLarge = InstanceType' "r5d.xlarge"

pattern R5dn_12XLarge :: InstanceType
pattern R5dn_12XLarge = InstanceType' "r5dn.12xlarge"

pattern R5dn_16XLarge :: InstanceType
pattern R5dn_16XLarge = InstanceType' "r5dn.16xlarge"

pattern R5dn_24XLarge :: InstanceType
pattern R5dn_24XLarge = InstanceType' "r5dn.24xlarge"

pattern R5dn_2XLarge :: InstanceType
pattern R5dn_2XLarge = InstanceType' "r5dn.2xlarge"

pattern R5dn_4XLarge :: InstanceType
pattern R5dn_4XLarge = InstanceType' "r5dn.4xlarge"

pattern R5dn_8XLarge :: InstanceType
pattern R5dn_8XLarge = InstanceType' "r5dn.8xlarge"

pattern R5dn_Large :: InstanceType
pattern R5dn_Large = InstanceType' "r5dn.large"

pattern R5dn_XLarge :: InstanceType
pattern R5dn_XLarge = InstanceType' "r5dn.xlarge"

pattern R5n_12XLarge :: InstanceType
pattern R5n_12XLarge = InstanceType' "r5n.12xlarge"

pattern R5n_16XLarge :: InstanceType
pattern R5n_16XLarge = InstanceType' "r5n.16xlarge"

pattern R5n_24XLarge :: InstanceType
pattern R5n_24XLarge = InstanceType' "r5n.24xlarge"

pattern R5n_2XLarge :: InstanceType
pattern R5n_2XLarge = InstanceType' "r5n.2xlarge"

pattern R5n_4XLarge :: InstanceType
pattern R5n_4XLarge = InstanceType' "r5n.4xlarge"

pattern R5n_8XLarge :: InstanceType
pattern R5n_8XLarge = InstanceType' "r5n.8xlarge"

pattern R5n_Large :: InstanceType
pattern R5n_Large = InstanceType' "r5n.large"

pattern R5n_XLarge :: InstanceType
pattern R5n_XLarge = InstanceType' "r5n.xlarge"

pattern T1_Micro :: InstanceType
pattern T1_Micro = InstanceType' "t1.micro"

pattern T2_2XLarge :: InstanceType
pattern T2_2XLarge = InstanceType' "t2.2xlarge"

pattern T2_Large :: InstanceType
pattern T2_Large = InstanceType' "t2.large"

pattern T2_Medium :: InstanceType
pattern T2_Medium = InstanceType' "t2.medium"

pattern T2_Micro :: InstanceType
pattern T2_Micro = InstanceType' "t2.micro"

pattern T2_Nano :: InstanceType
pattern T2_Nano = InstanceType' "t2.nano"

pattern T2_Small :: InstanceType
pattern T2_Small = InstanceType' "t2.small"

pattern T2_XLarge :: InstanceType
pattern T2_XLarge = InstanceType' "t2.xlarge"

pattern T3_2XLarge :: InstanceType
pattern T3_2XLarge = InstanceType' "t3.2xlarge"

pattern T3_Large :: InstanceType
pattern T3_Large = InstanceType' "t3.large"

pattern T3_Medium :: InstanceType
pattern T3_Medium = InstanceType' "t3.medium"

pattern T3_Micro :: InstanceType
pattern T3_Micro = InstanceType' "t3.micro"

pattern T3_Nano :: InstanceType
pattern T3_Nano = InstanceType' "t3.nano"

pattern T3_Small :: InstanceType
pattern T3_Small = InstanceType' "t3.small"

pattern T3_XLarge :: InstanceType
pattern T3_XLarge = InstanceType' "t3.xlarge"

pattern T3a_2XLarge :: InstanceType
pattern T3a_2XLarge = InstanceType' "t3a.2xlarge"

pattern T3a_Large :: InstanceType
pattern T3a_Large = InstanceType' "t3a.large"

pattern T3a_Medium :: InstanceType
pattern T3a_Medium = InstanceType' "t3a.medium"

pattern T3a_Micro :: InstanceType
pattern T3a_Micro = InstanceType' "t3a.micro"

pattern T3a_Nano :: InstanceType
pattern T3a_Nano = InstanceType' "t3a.nano"

pattern T3a_Small :: InstanceType
pattern T3a_Small = InstanceType' "t3a.small"

pattern T3a_XLarge :: InstanceType
pattern T3a_XLarge = InstanceType' "t3a.xlarge"

pattern U12TB1_Metal :: InstanceType
pattern U12TB1_Metal = InstanceType' "u-12tb1.metal"

pattern U18TB1_Metal :: InstanceType
pattern U18TB1_Metal = InstanceType' "u-18tb1.metal"

pattern U24TB1_Metal :: InstanceType
pattern U24TB1_Metal = InstanceType' "u-24tb1.metal"

pattern U6TB1_Metal :: InstanceType
pattern U6TB1_Metal = InstanceType' "u-6tb1.metal"

pattern U9TB1_Metal :: InstanceType
pattern U9TB1_Metal = InstanceType' "u-9tb1.metal"

pattern X1_16XLarge :: InstanceType
pattern X1_16XLarge = InstanceType' "x1.16xlarge"

pattern X1_32XLarge :: InstanceType
pattern X1_32XLarge = InstanceType' "x1.32xlarge"

pattern X1e_16XLarge :: InstanceType
pattern X1e_16XLarge = InstanceType' "x1e.16xlarge"

pattern X1e_2XLarge :: InstanceType
pattern X1e_2XLarge = InstanceType' "x1e.2xlarge"

pattern X1e_32XLarge :: InstanceType
pattern X1e_32XLarge = InstanceType' "x1e.32xlarge"

pattern X1e_4XLarge :: InstanceType
pattern X1e_4XLarge = InstanceType' "x1e.4xlarge"

pattern X1e_8XLarge :: InstanceType
pattern X1e_8XLarge = InstanceType' "x1e.8xlarge"

pattern X1e_XLarge :: InstanceType
pattern X1e_XLarge = InstanceType' "x1e.xlarge"

pattern Z1d_12XLarge :: InstanceType
pattern Z1d_12XLarge = InstanceType' "z1d.12xlarge"

pattern Z1d_2XLarge :: InstanceType
pattern Z1d_2XLarge = InstanceType' "z1d.2xlarge"

pattern Z1d_3XLarge :: InstanceType
pattern Z1d_3XLarge = InstanceType' "z1d.3xlarge"

pattern Z1d_6XLarge :: InstanceType
pattern Z1d_6XLarge = InstanceType' "z1d.6xlarge"

pattern Z1d_Large :: InstanceType
pattern Z1d_Large = InstanceType' "z1d.large"

pattern Z1d_Metal :: InstanceType
pattern Z1d_Metal = InstanceType' "z1d.metal"

pattern Z1d_XLarge :: InstanceType
pattern Z1d_XLarge = InstanceType' "z1d.xlarge"

{-# COMPLETE
  A1_2XLarge,
  A1_4XLarge,
  A1_Large,
  A1_Medium,
  A1_Metal,
  A1_XLarge,
  C1_Medium,
  C1_XLarge,
  C3_2XLarge,
  C3_4XLarge,
  C3_8XLarge,
  C3_Large,
  C3_XLarge,
  C4_2XLarge,
  C4_4XLarge,
  C4_8XLarge,
  C4_Large,
  C4_XLarge,
  C5_12XLarge,
  C5_18XLarge,
  C5_24XLarge,
  C5_2XLarge,
  C5_4XLarge,
  C5_9XLarge,
  C5_Large,
  C5_Metal,
  C5_XLarge,
  C5d_12XLarge,
  C5d_18XLarge,
  C5d_24XLarge,
  C5d_2XLarge,
  C5d_4XLarge,
  C5d_9XLarge,
  C5d_Large,
  C5d_Metal,
  C5d_XLarge,
  C5n_18XLarge,
  C5n_2XLarge,
  C5n_4XLarge,
  C5n_9XLarge,
  C5n_Large,
  C5n_XLarge,
  CC1_4XLarge,
  CC2_8XLarge,
  CG1_4XLarge,
  CR1_8XLarge,
  D2_2XLarge,
  D2_4XLarge,
  D2_8XLarge,
  D2_XLarge,
  F1_16XLarge,
  F1_2XLarge,
  F1_4XLarge,
  G2_2XLarge,
  G2_8XLarge,
  G3_16XLarge,
  G3_4XLarge,
  G3_8XLarge,
  G3s_XLarge,
  G4dn_12XLarge,
  G4dn_16XLarge,
  G4dn_2XLarge,
  G4dn_4XLarge,
  G4dn_8XLarge,
  G4dn_XLarge,
  H1_16XLarge,
  H1_2XLarge,
  H1_4XLarge,
  H1_8XLarge,
  HI1_4XLarge,
  HS1_8XLarge,
  I2_2XLarge,
  I2_4XLarge,
  I2_8XLarge,
  I2_XLarge,
  I3_16XLarge,
  I3_2XLarge,
  I3_4XLarge,
  I3_8XLarge,
  I3_Large,
  I3_Metal,
  I3_XLarge,
  I3en_12XLarge,
  I3en_24XLarge,
  I3en_2XLarge,
  I3en_3XLarge,
  I3en_6XLarge,
  I3en_Large,
  I3en_Metal,
  I3en_XLarge,
  INF1_24XLarge,
  INF1_2XLarge,
  INF1_6XLarge,
  INF1_XLarge,
  M1_Large,
  M1_Medium,
  M1_Small,
  M1_XLarge,
  M2_2XLarge,
  M2_4XLarge,
  M2_XLarge,
  M3_2XLarge,
  M3_Large,
  M3_Medium,
  M3_XLarge,
  M4_10XLarge,
  M4_16XLarge,
  M4_2XLarge,
  M4_4XLarge,
  M4_Large,
  M4_XLarge,
  M5_12XLarge,
  M5_16XLarge,
  M5_24XLarge,
  M5_2XLarge,
  M5_4XLarge,
  M5_8XLarge,
  M5_Large,
  M5_Metal,
  M5_XLarge,
  M5a_12XLarge,
  M5a_16XLarge,
  M5a_24XLarge,
  M5a_2XLarge,
  M5a_4XLarge,
  M5a_8XLarge,
  M5a_Large,
  M5a_XLarge,
  M5ad_12XLarge,
  M5ad_16XLarge,
  M5ad_24XLarge,
  M5ad_2XLarge,
  M5ad_4XLarge,
  M5ad_8XLarge,
  M5ad_Large,
  M5ad_XLarge,
  M5d_12XLarge,
  M5d_16XLarge,
  M5d_24XLarge,
  M5d_2XLarge,
  M5d_4XLarge,
  M5d_8XLarge,
  M5d_Large,
  M5d_Metal,
  M5d_XLarge,
  M5dn_12XLarge,
  M5dn_16XLarge,
  M5dn_24XLarge,
  M5dn_2XLarge,
  M5dn_4XLarge,
  M5dn_8XLarge,
  M5dn_Large,
  M5dn_XLarge,
  M5n_12XLarge,
  M5n_16XLarge,
  M5n_24XLarge,
  M5n_2XLarge,
  M5n_4XLarge,
  M5n_8XLarge,
  M5n_Large,
  M5n_XLarge,
  P2_16XLarge,
  P2_8XLarge,
  P2_XLarge,
  P3_16XLarge,
  P3_2XLarge,
  P3_8XLarge,
  P3dn_24XLarge,
  R3_2XLarge,
  R3_4XLarge,
  R3_8XLarge,
  R3_Large,
  R3_XLarge,
  R4_16XLarge,
  R4_2XLarge,
  R4_4XLarge,
  R4_8XLarge,
  R4_Large,
  R4_XLarge,
  R5_12XLarge,
  R5_16XLarge,
  R5_24XLarge,
  R5_2XLarge,
  R5_4XLarge,
  R5_8XLarge,
  R5_Large,
  R5_Metal,
  R5_XLarge,
  R5a_12XLarge,
  R5a_16XLarge,
  R5a_24XLarge,
  R5a_2XLarge,
  R5a_4XLarge,
  R5a_8XLarge,
  R5a_Large,
  R5a_XLarge,
  R5ad_12XLarge,
  R5ad_16XLarge,
  R5ad_24XLarge,
  R5ad_2XLarge,
  R5ad_4XLarge,
  R5ad_8XLarge,
  R5ad_Large,
  R5ad_XLarge,
  R5d_12XLarge,
  R5d_16XLarge,
  R5d_24XLarge,
  R5d_2XLarge,
  R5d_4XLarge,
  R5d_8XLarge,
  R5d_Large,
  R5d_Metal,
  R5d_XLarge,
  R5dn_12XLarge,
  R5dn_16XLarge,
  R5dn_24XLarge,
  R5dn_2XLarge,
  R5dn_4XLarge,
  R5dn_8XLarge,
  R5dn_Large,
  R5dn_XLarge,
  R5n_12XLarge,
  R5n_16XLarge,
  R5n_24XLarge,
  R5n_2XLarge,
  R5n_4XLarge,
  R5n_8XLarge,
  R5n_Large,
  R5n_XLarge,
  T1_Micro,
  T2_2XLarge,
  T2_Large,
  T2_Medium,
  T2_Micro,
  T2_Nano,
  T2_Small,
  T2_XLarge,
  T3_2XLarge,
  T3_Large,
  T3_Medium,
  T3_Micro,
  T3_Nano,
  T3_Small,
  T3_XLarge,
  T3a_2XLarge,
  T3a_Large,
  T3a_Medium,
  T3a_Micro,
  T3a_Nano,
  T3a_Small,
  T3a_XLarge,
  U12TB1_Metal,
  U18TB1_Metal,
  U24TB1_Metal,
  U6TB1_Metal,
  U9TB1_Metal,
  X1_16XLarge,
  X1_32XLarge,
  X1e_16XLarge,
  X1e_2XLarge,
  X1e_32XLarge,
  X1e_4XLarge,
  X1e_8XLarge,
  X1e_XLarge,
  Z1d_12XLarge,
  Z1d_2XLarge,
  Z1d_3XLarge,
  Z1d_6XLarge,
  Z1d_Large,
  Z1d_Metal,
  Z1d_XLarge,
  InstanceType' #-}

instance FromText InstanceType where
    parser = (InstanceType' . mk) <$> takeText

instance ToText InstanceType where
    toText (InstanceType' ci) = original ci

-- | Represents an enum of /known/ $InstanceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InstanceType where
    toEnum i = case i of
        0 -> A1_2XLarge
        1 -> A1_4XLarge
        2 -> A1_Large
        3 -> A1_Medium
        4 -> A1_Metal
        5 -> A1_XLarge
        6 -> C1_Medium
        7 -> C1_XLarge
        8 -> C3_2XLarge
        9 -> C3_4XLarge
        10 -> C3_8XLarge
        11 -> C3_Large
        12 -> C3_XLarge
        13 -> C4_2XLarge
        14 -> C4_4XLarge
        15 -> C4_8XLarge
        16 -> C4_Large
        17 -> C4_XLarge
        18 -> C5_12XLarge
        19 -> C5_18XLarge
        20 -> C5_24XLarge
        21 -> C5_2XLarge
        22 -> C5_4XLarge
        23 -> C5_9XLarge
        24 -> C5_Large
        25 -> C5_Metal
        26 -> C5_XLarge
        27 -> C5d_12XLarge
        28 -> C5d_18XLarge
        29 -> C5d_24XLarge
        30 -> C5d_2XLarge
        31 -> C5d_4XLarge
        32 -> C5d_9XLarge
        33 -> C5d_Large
        34 -> C5d_Metal
        35 -> C5d_XLarge
        36 -> C5n_18XLarge
        37 -> C5n_2XLarge
        38 -> C5n_4XLarge
        39 -> C5n_9XLarge
        40 -> C5n_Large
        41 -> C5n_XLarge
        42 -> CC1_4XLarge
        43 -> CC2_8XLarge
        44 -> CG1_4XLarge
        45 -> CR1_8XLarge
        46 -> D2_2XLarge
        47 -> D2_4XLarge
        48 -> D2_8XLarge
        49 -> D2_XLarge
        50 -> F1_16XLarge
        51 -> F1_2XLarge
        52 -> F1_4XLarge
        53 -> G2_2XLarge
        54 -> G2_8XLarge
        55 -> G3_16XLarge
        56 -> G3_4XLarge
        57 -> G3_8XLarge
        58 -> G3s_XLarge
        59 -> G4dn_12XLarge
        60 -> G4dn_16XLarge
        61 -> G4dn_2XLarge
        62 -> G4dn_4XLarge
        63 -> G4dn_8XLarge
        64 -> G4dn_XLarge
        65 -> H1_16XLarge
        66 -> H1_2XLarge
        67 -> H1_4XLarge
        68 -> H1_8XLarge
        69 -> HI1_4XLarge
        70 -> HS1_8XLarge
        71 -> I2_2XLarge
        72 -> I2_4XLarge
        73 -> I2_8XLarge
        74 -> I2_XLarge
        75 -> I3_16XLarge
        76 -> I3_2XLarge
        77 -> I3_4XLarge
        78 -> I3_8XLarge
        79 -> I3_Large
        80 -> I3_Metal
        81 -> I3_XLarge
        82 -> I3en_12XLarge
        83 -> I3en_24XLarge
        84 -> I3en_2XLarge
        85 -> I3en_3XLarge
        86 -> I3en_6XLarge
        87 -> I3en_Large
        88 -> I3en_Metal
        89 -> I3en_XLarge
        90 -> INF1_24XLarge
        91 -> INF1_2XLarge
        92 -> INF1_6XLarge
        93 -> INF1_XLarge
        94 -> M1_Large
        95 -> M1_Medium
        96 -> M1_Small
        97 -> M1_XLarge
        98 -> M2_2XLarge
        99 -> M2_4XLarge
        100 -> M2_XLarge
        101 -> M3_2XLarge
        102 -> M3_Large
        103 -> M3_Medium
        104 -> M3_XLarge
        105 -> M4_10XLarge
        106 -> M4_16XLarge
        107 -> M4_2XLarge
        108 -> M4_4XLarge
        109 -> M4_Large
        110 -> M4_XLarge
        111 -> M5_12XLarge
        112 -> M5_16XLarge
        113 -> M5_24XLarge
        114 -> M5_2XLarge
        115 -> M5_4XLarge
        116 -> M5_8XLarge
        117 -> M5_Large
        118 -> M5_Metal
        119 -> M5_XLarge
        120 -> M5a_12XLarge
        121 -> M5a_16XLarge
        122 -> M5a_24XLarge
        123 -> M5a_2XLarge
        124 -> M5a_4XLarge
        125 -> M5a_8XLarge
        126 -> M5a_Large
        127 -> M5a_XLarge
        128 -> M5ad_12XLarge
        129 -> M5ad_16XLarge
        130 -> M5ad_24XLarge
        131 -> M5ad_2XLarge
        132 -> M5ad_4XLarge
        133 -> M5ad_8XLarge
        134 -> M5ad_Large
        135 -> M5ad_XLarge
        136 -> M5d_12XLarge
        137 -> M5d_16XLarge
        138 -> M5d_24XLarge
        139 -> M5d_2XLarge
        140 -> M5d_4XLarge
        141 -> M5d_8XLarge
        142 -> M5d_Large
        143 -> M5d_Metal
        144 -> M5d_XLarge
        145 -> M5dn_12XLarge
        146 -> M5dn_16XLarge
        147 -> M5dn_24XLarge
        148 -> M5dn_2XLarge
        149 -> M5dn_4XLarge
        150 -> M5dn_8XLarge
        151 -> M5dn_Large
        152 -> M5dn_XLarge
        153 -> M5n_12XLarge
        154 -> M5n_16XLarge
        155 -> M5n_24XLarge
        156 -> M5n_2XLarge
        157 -> M5n_4XLarge
        158 -> M5n_8XLarge
        159 -> M5n_Large
        160 -> M5n_XLarge
        161 -> P2_16XLarge
        162 -> P2_8XLarge
        163 -> P2_XLarge
        164 -> P3_16XLarge
        165 -> P3_2XLarge
        166 -> P3_8XLarge
        167 -> P3dn_24XLarge
        168 -> R3_2XLarge
        169 -> R3_4XLarge
        170 -> R3_8XLarge
        171 -> R3_Large
        172 -> R3_XLarge
        173 -> R4_16XLarge
        174 -> R4_2XLarge
        175 -> R4_4XLarge
        176 -> R4_8XLarge
        177 -> R4_Large
        178 -> R4_XLarge
        179 -> R5_12XLarge
        180 -> R5_16XLarge
        181 -> R5_24XLarge
        182 -> R5_2XLarge
        183 -> R5_4XLarge
        184 -> R5_8XLarge
        185 -> R5_Large
        186 -> R5_Metal
        187 -> R5_XLarge
        188 -> R5a_12XLarge
        189 -> R5a_16XLarge
        190 -> R5a_24XLarge
        191 -> R5a_2XLarge
        192 -> R5a_4XLarge
        193 -> R5a_8XLarge
        194 -> R5a_Large
        195 -> R5a_XLarge
        196 -> R5ad_12XLarge
        197 -> R5ad_16XLarge
        198 -> R5ad_24XLarge
        199 -> R5ad_2XLarge
        200 -> R5ad_4XLarge
        201 -> R5ad_8XLarge
        202 -> R5ad_Large
        203 -> R5ad_XLarge
        204 -> R5d_12XLarge
        205 -> R5d_16XLarge
        206 -> R5d_24XLarge
        207 -> R5d_2XLarge
        208 -> R5d_4XLarge
        209 -> R5d_8XLarge
        210 -> R5d_Large
        211 -> R5d_Metal
        212 -> R5d_XLarge
        213 -> R5dn_12XLarge
        214 -> R5dn_16XLarge
        215 -> R5dn_24XLarge
        216 -> R5dn_2XLarge
        217 -> R5dn_4XLarge
        218 -> R5dn_8XLarge
        219 -> R5dn_Large
        220 -> R5dn_XLarge
        221 -> R5n_12XLarge
        222 -> R5n_16XLarge
        223 -> R5n_24XLarge
        224 -> R5n_2XLarge
        225 -> R5n_4XLarge
        226 -> R5n_8XLarge
        227 -> R5n_Large
        228 -> R5n_XLarge
        229 -> T1_Micro
        230 -> T2_2XLarge
        231 -> T2_Large
        232 -> T2_Medium
        233 -> T2_Micro
        234 -> T2_Nano
        235 -> T2_Small
        236 -> T2_XLarge
        237 -> T3_2XLarge
        238 -> T3_Large
        239 -> T3_Medium
        240 -> T3_Micro
        241 -> T3_Nano
        242 -> T3_Small
        243 -> T3_XLarge
        244 -> T3a_2XLarge
        245 -> T3a_Large
        246 -> T3a_Medium
        247 -> T3a_Micro
        248 -> T3a_Nano
        249 -> T3a_Small
        250 -> T3a_XLarge
        251 -> U12TB1_Metal
        252 -> U18TB1_Metal
        253 -> U24TB1_Metal
        254 -> U6TB1_Metal
        255 -> U9TB1_Metal
        256 -> X1_16XLarge
        257 -> X1_32XLarge
        258 -> X1e_16XLarge
        259 -> X1e_2XLarge
        260 -> X1e_32XLarge
        261 -> X1e_4XLarge
        262 -> X1e_8XLarge
        263 -> X1e_XLarge
        264 -> Z1d_12XLarge
        265 -> Z1d_2XLarge
        266 -> Z1d_3XLarge
        267 -> Z1d_6XLarge
        268 -> Z1d_Large
        269 -> Z1d_Metal
        270 -> Z1d_XLarge
        _ -> (error . showText) $ "Unknown index for InstanceType: " <> toText i
    fromEnum x = case x of
        A1_2XLarge -> 0
        A1_4XLarge -> 1
        A1_Large -> 2
        A1_Medium -> 3
        A1_Metal -> 4
        A1_XLarge -> 5
        C1_Medium -> 6
        C1_XLarge -> 7
        C3_2XLarge -> 8
        C3_4XLarge -> 9
        C3_8XLarge -> 10
        C3_Large -> 11
        C3_XLarge -> 12
        C4_2XLarge -> 13
        C4_4XLarge -> 14
        C4_8XLarge -> 15
        C4_Large -> 16
        C4_XLarge -> 17
        C5_12XLarge -> 18
        C5_18XLarge -> 19
        C5_24XLarge -> 20
        C5_2XLarge -> 21
        C5_4XLarge -> 22
        C5_9XLarge -> 23
        C5_Large -> 24
        C5_Metal -> 25
        C5_XLarge -> 26
        C5d_12XLarge -> 27
        C5d_18XLarge -> 28
        C5d_24XLarge -> 29
        C5d_2XLarge -> 30
        C5d_4XLarge -> 31
        C5d_9XLarge -> 32
        C5d_Large -> 33
        C5d_Metal -> 34
        C5d_XLarge -> 35
        C5n_18XLarge -> 36
        C5n_2XLarge -> 37
        C5n_4XLarge -> 38
        C5n_9XLarge -> 39
        C5n_Large -> 40
        C5n_XLarge -> 41
        CC1_4XLarge -> 42
        CC2_8XLarge -> 43
        CG1_4XLarge -> 44
        CR1_8XLarge -> 45
        D2_2XLarge -> 46
        D2_4XLarge -> 47
        D2_8XLarge -> 48
        D2_XLarge -> 49
        F1_16XLarge -> 50
        F1_2XLarge -> 51
        F1_4XLarge -> 52
        G2_2XLarge -> 53
        G2_8XLarge -> 54
        G3_16XLarge -> 55
        G3_4XLarge -> 56
        G3_8XLarge -> 57
        G3s_XLarge -> 58
        G4dn_12XLarge -> 59
        G4dn_16XLarge -> 60
        G4dn_2XLarge -> 61
        G4dn_4XLarge -> 62
        G4dn_8XLarge -> 63
        G4dn_XLarge -> 64
        H1_16XLarge -> 65
        H1_2XLarge -> 66
        H1_4XLarge -> 67
        H1_8XLarge -> 68
        HI1_4XLarge -> 69
        HS1_8XLarge -> 70
        I2_2XLarge -> 71
        I2_4XLarge -> 72
        I2_8XLarge -> 73
        I2_XLarge -> 74
        I3_16XLarge -> 75
        I3_2XLarge -> 76
        I3_4XLarge -> 77
        I3_8XLarge -> 78
        I3_Large -> 79
        I3_Metal -> 80
        I3_XLarge -> 81
        I3en_12XLarge -> 82
        I3en_24XLarge -> 83
        I3en_2XLarge -> 84
        I3en_3XLarge -> 85
        I3en_6XLarge -> 86
        I3en_Large -> 87
        I3en_Metal -> 88
        I3en_XLarge -> 89
        INF1_24XLarge -> 90
        INF1_2XLarge -> 91
        INF1_6XLarge -> 92
        INF1_XLarge -> 93
        M1_Large -> 94
        M1_Medium -> 95
        M1_Small -> 96
        M1_XLarge -> 97
        M2_2XLarge -> 98
        M2_4XLarge -> 99
        M2_XLarge -> 100
        M3_2XLarge -> 101
        M3_Large -> 102
        M3_Medium -> 103
        M3_XLarge -> 104
        M4_10XLarge -> 105
        M4_16XLarge -> 106
        M4_2XLarge -> 107
        M4_4XLarge -> 108
        M4_Large -> 109
        M4_XLarge -> 110
        M5_12XLarge -> 111
        M5_16XLarge -> 112
        M5_24XLarge -> 113
        M5_2XLarge -> 114
        M5_4XLarge -> 115
        M5_8XLarge -> 116
        M5_Large -> 117
        M5_Metal -> 118
        M5_XLarge -> 119
        M5a_12XLarge -> 120
        M5a_16XLarge -> 121
        M5a_24XLarge -> 122
        M5a_2XLarge -> 123
        M5a_4XLarge -> 124
        M5a_8XLarge -> 125
        M5a_Large -> 126
        M5a_XLarge -> 127
        M5ad_12XLarge -> 128
        M5ad_16XLarge -> 129
        M5ad_24XLarge -> 130
        M5ad_2XLarge -> 131
        M5ad_4XLarge -> 132
        M5ad_8XLarge -> 133
        M5ad_Large -> 134
        M5ad_XLarge -> 135
        M5d_12XLarge -> 136
        M5d_16XLarge -> 137
        M5d_24XLarge -> 138
        M5d_2XLarge -> 139
        M5d_4XLarge -> 140
        M5d_8XLarge -> 141
        M5d_Large -> 142
        M5d_Metal -> 143
        M5d_XLarge -> 144
        M5dn_12XLarge -> 145
        M5dn_16XLarge -> 146
        M5dn_24XLarge -> 147
        M5dn_2XLarge -> 148
        M5dn_4XLarge -> 149
        M5dn_8XLarge -> 150
        M5dn_Large -> 151
        M5dn_XLarge -> 152
        M5n_12XLarge -> 153
        M5n_16XLarge -> 154
        M5n_24XLarge -> 155
        M5n_2XLarge -> 156
        M5n_4XLarge -> 157
        M5n_8XLarge -> 158
        M5n_Large -> 159
        M5n_XLarge -> 160
        P2_16XLarge -> 161
        P2_8XLarge -> 162
        P2_XLarge -> 163
        P3_16XLarge -> 164
        P3_2XLarge -> 165
        P3_8XLarge -> 166
        P3dn_24XLarge -> 167
        R3_2XLarge -> 168
        R3_4XLarge -> 169
        R3_8XLarge -> 170
        R3_Large -> 171
        R3_XLarge -> 172
        R4_16XLarge -> 173
        R4_2XLarge -> 174
        R4_4XLarge -> 175
        R4_8XLarge -> 176
        R4_Large -> 177
        R4_XLarge -> 178
        R5_12XLarge -> 179
        R5_16XLarge -> 180
        R5_24XLarge -> 181
        R5_2XLarge -> 182
        R5_4XLarge -> 183
        R5_8XLarge -> 184
        R5_Large -> 185
        R5_Metal -> 186
        R5_XLarge -> 187
        R5a_12XLarge -> 188
        R5a_16XLarge -> 189
        R5a_24XLarge -> 190
        R5a_2XLarge -> 191
        R5a_4XLarge -> 192
        R5a_8XLarge -> 193
        R5a_Large -> 194
        R5a_XLarge -> 195
        R5ad_12XLarge -> 196
        R5ad_16XLarge -> 197
        R5ad_24XLarge -> 198
        R5ad_2XLarge -> 199
        R5ad_4XLarge -> 200
        R5ad_8XLarge -> 201
        R5ad_Large -> 202
        R5ad_XLarge -> 203
        R5d_12XLarge -> 204
        R5d_16XLarge -> 205
        R5d_24XLarge -> 206
        R5d_2XLarge -> 207
        R5d_4XLarge -> 208
        R5d_8XLarge -> 209
        R5d_Large -> 210
        R5d_Metal -> 211
        R5d_XLarge -> 212
        R5dn_12XLarge -> 213
        R5dn_16XLarge -> 214
        R5dn_24XLarge -> 215
        R5dn_2XLarge -> 216
        R5dn_4XLarge -> 217
        R5dn_8XLarge -> 218
        R5dn_Large -> 219
        R5dn_XLarge -> 220
        R5n_12XLarge -> 221
        R5n_16XLarge -> 222
        R5n_24XLarge -> 223
        R5n_2XLarge -> 224
        R5n_4XLarge -> 225
        R5n_8XLarge -> 226
        R5n_Large -> 227
        R5n_XLarge -> 228
        T1_Micro -> 229
        T2_2XLarge -> 230
        T2_Large -> 231
        T2_Medium -> 232
        T2_Micro -> 233
        T2_Nano -> 234
        T2_Small -> 235
        T2_XLarge -> 236
        T3_2XLarge -> 237
        T3_Large -> 238
        T3_Medium -> 239
        T3_Micro -> 240
        T3_Nano -> 241
        T3_Small -> 242
        T3_XLarge -> 243
        T3a_2XLarge -> 244
        T3a_Large -> 245
        T3a_Medium -> 246
        T3a_Micro -> 247
        T3a_Nano -> 248
        T3a_Small -> 249
        T3a_XLarge -> 250
        U12TB1_Metal -> 251
        U18TB1_Metal -> 252
        U24TB1_Metal -> 253
        U6TB1_Metal -> 254
        U9TB1_Metal -> 255
        X1_16XLarge -> 256
        X1_32XLarge -> 257
        X1e_16XLarge -> 258
        X1e_2XLarge -> 259
        X1e_32XLarge -> 260
        X1e_4XLarge -> 261
        X1e_8XLarge -> 262
        X1e_XLarge -> 263
        Z1d_12XLarge -> 264
        Z1d_2XLarge -> 265
        Z1d_3XLarge -> 266
        Z1d_6XLarge -> 267
        Z1d_Large -> 268
        Z1d_Metal -> 269
        Z1d_XLarge -> 270
        InstanceType' name -> (error . showText) $ "Unknown InstanceType: " <> original name

-- | Represents the bounds of /known/ $InstanceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InstanceType where
    minBound = A1_2XLarge
    maxBound = Z1d_XLarge

instance Hashable     InstanceType
instance NFData       InstanceType
instance ToByteString InstanceType
instance ToQuery      InstanceType
instance ToHeader     InstanceType

instance FromXML InstanceType where
    parseXML = parseXMLText "InstanceType"
