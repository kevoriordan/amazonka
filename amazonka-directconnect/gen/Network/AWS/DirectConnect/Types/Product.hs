{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.Product (
    module Network.AWS.DirectConnect.Types.AssociatedGateway,
    module Network.AWS.DirectConnect.Types.BGPPeer,
    module Network.AWS.DirectConnect.Types.Connection,
    module Network.AWS.DirectConnect.Types.Connections,
    module Network.AWS.DirectConnect.Types.DirectConnectGateway,
    module Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociation,
    module Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationProposal,
    module Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachment,
    module Network.AWS.DirectConnect.Types.Interconnect,
    module Network.AWS.DirectConnect.Types.Lag,
    module Network.AWS.DirectConnect.Types.Location,
    module Network.AWS.DirectConnect.Types.NewBGPPeer,
    module Network.AWS.DirectConnect.Types.NewPrivateVirtualInterface,
    module Network.AWS.DirectConnect.Types.NewPrivateVirtualInterfaceAllocation,
    module Network.AWS.DirectConnect.Types.NewPublicVirtualInterface,
    module Network.AWS.DirectConnect.Types.NewPublicVirtualInterfaceAllocation,
    module Network.AWS.DirectConnect.Types.NewTransitVirtualInterface,
    module Network.AWS.DirectConnect.Types.NewTransitVirtualInterfaceAllocation,
    module Network.AWS.DirectConnect.Types.ResourceTag,
    module Network.AWS.DirectConnect.Types.RouteFilterPrefix,
    module Network.AWS.DirectConnect.Types.Tag,
    module Network.AWS.DirectConnect.Types.VirtualGateway,
    module Network.AWS.DirectConnect.Types.VirtualInterface
  ) where

import Network.AWS.DirectConnect.Types.AssociatedGateway
import Network.AWS.DirectConnect.Types.BGPPeer
import Network.AWS.DirectConnect.Types.Connection
import Network.AWS.DirectConnect.Types.Connections
import Network.AWS.DirectConnect.Types.DirectConnectGateway
import Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociation
import Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationProposal
import Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachment
import Network.AWS.DirectConnect.Types.Interconnect
import Network.AWS.DirectConnect.Types.Lag
import Network.AWS.DirectConnect.Types.Location
import Network.AWS.DirectConnect.Types.NewBGPPeer
import Network.AWS.DirectConnect.Types.NewPrivateVirtualInterface
import Network.AWS.DirectConnect.Types.NewPrivateVirtualInterfaceAllocation
import Network.AWS.DirectConnect.Types.NewPublicVirtualInterface
import Network.AWS.DirectConnect.Types.NewPublicVirtualInterfaceAllocation
import Network.AWS.DirectConnect.Types.NewTransitVirtualInterface
import Network.AWS.DirectConnect.Types.NewTransitVirtualInterfaceAllocation
import Network.AWS.DirectConnect.Types.ResourceTag
import Network.AWS.DirectConnect.Types.RouteFilterPrefix
import Network.AWS.DirectConnect.Types.Tag
import Network.AWS.DirectConnect.Types.VirtualGateway
import Network.AWS.DirectConnect.Types.VirtualInterface
import Network.AWS.Lens
import Network.AWS.Prelude
