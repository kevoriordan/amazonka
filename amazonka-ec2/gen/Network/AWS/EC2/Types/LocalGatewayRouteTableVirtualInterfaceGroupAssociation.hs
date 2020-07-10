{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LocalGatewayRouteTableVirtualInterfaceGroupAssociation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LocalGatewayRouteTableVirtualInterfaceGroupAssociation where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an association between a local gateway route table and a virtual interface group.
--
--
--
-- /See:/ 'localGatewayRouteTableVirtualInterfaceGroupAssociation' smart constructor.
data LocalGatewayRouteTableVirtualInterfaceGroupAssociation = LocalGatewayRouteTableVirtualInterfaceGroupAssociation'{_lgrtvigaState
                                                                                                                      ::
                                                                                                                      !(Maybe
                                                                                                                          Text),
                                                                                                                      _lgrtvigaLocalGatewayId
                                                                                                                      ::
                                                                                                                      !(Maybe
                                                                                                                          Text),
                                                                                                                      _lgrtvigaLocalGatewayRouteTableId
                                                                                                                      ::
                                                                                                                      !(Maybe
                                                                                                                          Text),
                                                                                                                      _lgrtvigaLocalGatewayRouteTableVirtualInterfaceGroupAssociationId
                                                                                                                      ::
                                                                                                                      !(Maybe
                                                                                                                          Text),
                                                                                                                      _lgrtvigaLocalGatewayVirtualInterfaceGroupId
                                                                                                                      ::
                                                                                                                      !(Maybe
                                                                                                                          Text),
                                                                                                                      _lgrtvigaTags
                                                                                                                      ::
                                                                                                                      !(Maybe
                                                                                                                          [Tag])}
                                                                deriving (Eq,
                                                                          Read,
                                                                          Show,
                                                                          Data,
                                                                          Typeable,
                                                                          Generic)

-- | Creates a value of 'LocalGatewayRouteTableVirtualInterfaceGroupAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgrtvigaState' - The state of the association.
--
-- * 'lgrtvigaLocalGatewayId' - The ID of the local gateway.
--
-- * 'lgrtvigaLocalGatewayRouteTableId' - The ID of the local gateway route table.
--
-- * 'lgrtvigaLocalGatewayRouteTableVirtualInterfaceGroupAssociationId' - The ID of the association.
--
-- * 'lgrtvigaLocalGatewayVirtualInterfaceGroupId' - The ID of the virtual interface group.
--
-- * 'lgrtvigaTags' - The tags assigned to the association.
localGatewayRouteTableVirtualInterfaceGroupAssociation
    :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation
localGatewayRouteTableVirtualInterfaceGroupAssociation
  = LocalGatewayRouteTableVirtualInterfaceGroupAssociation'{_lgrtvigaState
                                                              = Nothing,
                                                            _lgrtvigaLocalGatewayId
                                                              = Nothing,
                                                            _lgrtvigaLocalGatewayRouteTableId
                                                              = Nothing,
                                                            _lgrtvigaLocalGatewayRouteTableVirtualInterfaceGroupAssociationId
                                                              = Nothing,
                                                            _lgrtvigaLocalGatewayVirtualInterfaceGroupId
                                                              = Nothing,
                                                            _lgrtvigaTags =
                                                              Nothing}

-- | The state of the association.
lgrtvigaState :: Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Maybe Text)
lgrtvigaState = lens _lgrtvigaState (\ s a -> s{_lgrtvigaState = a})

-- | The ID of the local gateway.
lgrtvigaLocalGatewayId :: Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Maybe Text)
lgrtvigaLocalGatewayId = lens _lgrtvigaLocalGatewayId (\ s a -> s{_lgrtvigaLocalGatewayId = a})

-- | The ID of the local gateway route table.
lgrtvigaLocalGatewayRouteTableId :: Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Maybe Text)
lgrtvigaLocalGatewayRouteTableId = lens _lgrtvigaLocalGatewayRouteTableId (\ s a -> s{_lgrtvigaLocalGatewayRouteTableId = a})

-- | The ID of the association.
lgrtvigaLocalGatewayRouteTableVirtualInterfaceGroupAssociationId :: Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Maybe Text)
lgrtvigaLocalGatewayRouteTableVirtualInterfaceGroupAssociationId = lens _lgrtvigaLocalGatewayRouteTableVirtualInterfaceGroupAssociationId (\ s a -> s{_lgrtvigaLocalGatewayRouteTableVirtualInterfaceGroupAssociationId = a})

-- | The ID of the virtual interface group.
lgrtvigaLocalGatewayVirtualInterfaceGroupId :: Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Maybe Text)
lgrtvigaLocalGatewayVirtualInterfaceGroupId = lens _lgrtvigaLocalGatewayVirtualInterfaceGroupId (\ s a -> s{_lgrtvigaLocalGatewayVirtualInterfaceGroupId = a})

-- | The tags assigned to the association.
lgrtvigaTags :: Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation [Tag]
lgrtvigaTags = lens _lgrtvigaTags (\ s a -> s{_lgrtvigaTags = a}) . _Default . _Coerce

instance FromXML
           LocalGatewayRouteTableVirtualInterfaceGroupAssociation
         where
        parseXML x
          = LocalGatewayRouteTableVirtualInterfaceGroupAssociation'
              <$>
              (x .@? "state") <*> (x .@? "localGatewayId") <*>
                (x .@? "localGatewayRouteTableId")
                <*>
                (x .@?
                   "localGatewayRouteTableVirtualInterfaceGroupAssociationId")
                <*> (x .@? "localGatewayVirtualInterfaceGroupId")
                <*>
                (x .@? "tagSet" .!@ mempty >>=
                   may (parseXMLList "item"))

instance Hashable
           LocalGatewayRouteTableVirtualInterfaceGroupAssociation
         where

instance NFData
           LocalGatewayRouteTableVirtualInterfaceGroupAssociation
         where
