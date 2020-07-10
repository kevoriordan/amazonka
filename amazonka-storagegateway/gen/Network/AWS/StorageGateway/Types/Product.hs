{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StorageGateway.Types.Product (
    module Network.AWS.StorageGateway.Types.AutomaticTapeCreationPolicyInfo,
    module Network.AWS.StorageGateway.Types.AutomaticTapeCreationRule,
    module Network.AWS.StorageGateway.Types.CachediSCSIVolume,
    module Network.AWS.StorageGateway.Types.ChapInfo,
    module Network.AWS.StorageGateway.Types.DeviceiSCSIAttributes,
    module Network.AWS.StorageGateway.Types.Disk,
    module Network.AWS.StorageGateway.Types.FileShareInfo,
    module Network.AWS.StorageGateway.Types.GatewayInfo,
    module Network.AWS.StorageGateway.Types.NFSFileShareDefaults,
    module Network.AWS.StorageGateway.Types.NFSFileShareInfo,
    module Network.AWS.StorageGateway.Types.NetworkInterface,
    module Network.AWS.StorageGateway.Types.SMBFileShareInfo,
    module Network.AWS.StorageGateway.Types.StorediSCSIVolume,
    module Network.AWS.StorageGateway.Types.Tag,
    module Network.AWS.StorageGateway.Types.Tape,
    module Network.AWS.StorageGateway.Types.TapeArchive,
    module Network.AWS.StorageGateway.Types.TapeInfo,
    module Network.AWS.StorageGateway.Types.TapeRecoveryPointInfo,
    module Network.AWS.StorageGateway.Types.VTLDevice,
    module Network.AWS.StorageGateway.Types.VolumeInfo,
    module Network.AWS.StorageGateway.Types.VolumeRecoveryPointInfo,
    module Network.AWS.StorageGateway.Types.VolumeiSCSIAttributes
  ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.StorageGateway.Types.AutomaticTapeCreationPolicyInfo
import Network.AWS.StorageGateway.Types.AutomaticTapeCreationRule
import Network.AWS.StorageGateway.Types.CachediSCSIVolume
import Network.AWS.StorageGateway.Types.ChapInfo
import Network.AWS.StorageGateway.Types.DeviceiSCSIAttributes
import Network.AWS.StorageGateway.Types.Disk
import Network.AWS.StorageGateway.Types.FileShareInfo
import Network.AWS.StorageGateway.Types.GatewayInfo
import Network.AWS.StorageGateway.Types.NFSFileShareDefaults
import Network.AWS.StorageGateway.Types.NFSFileShareInfo
import Network.AWS.StorageGateway.Types.NetworkInterface
import Network.AWS.StorageGateway.Types.SMBFileShareInfo
import Network.AWS.StorageGateway.Types.StorediSCSIVolume
import Network.AWS.StorageGateway.Types.Tag
import Network.AWS.StorageGateway.Types.Tape
import Network.AWS.StorageGateway.Types.TapeArchive
import Network.AWS.StorageGateway.Types.TapeInfo
import Network.AWS.StorageGateway.Types.TapeRecoveryPointInfo
import Network.AWS.StorageGateway.Types.VTLDevice
import Network.AWS.StorageGateway.Types.VolumeInfo
import Network.AWS.StorageGateway.Types.VolumeRecoveryPointInfo
import Network.AWS.StorageGateway.Types.VolumeiSCSIAttributes
