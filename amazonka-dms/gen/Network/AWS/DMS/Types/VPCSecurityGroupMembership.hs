{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.VPCSecurityGroupMembership
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.VPCSecurityGroupMembership where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes status of a security group associated with the virtual private cloud hosting your replication and DB instances.
--
--
--
-- /See:/ 'vpcSecurityGroupMembership' smart constructor.
data VPCSecurityGroupMembership = VPCSecurityGroupMembership'{_vsgmStatus
                                                              :: !(Maybe Text),
                                                              _vsgmVPCSecurityGroupId
                                                              :: !(Maybe Text)}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'VPCSecurityGroupMembership' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vsgmStatus' - The status of the VPC security group.
--
-- * 'vsgmVPCSecurityGroupId' - The VPC security group Id.
vpcSecurityGroupMembership
    :: VPCSecurityGroupMembership
vpcSecurityGroupMembership
  = VPCSecurityGroupMembership'{_vsgmStatus = Nothing,
                                _vsgmVPCSecurityGroupId = Nothing}

-- | The status of the VPC security group.
vsgmStatus :: Lens' VPCSecurityGroupMembership (Maybe Text)
vsgmStatus = lens _vsgmStatus (\ s a -> s{_vsgmStatus = a})

-- | The VPC security group Id.
vsgmVPCSecurityGroupId :: Lens' VPCSecurityGroupMembership (Maybe Text)
vsgmVPCSecurityGroupId = lens _vsgmVPCSecurityGroupId (\ s a -> s{_vsgmVPCSecurityGroupId = a})

instance FromJSON VPCSecurityGroupMembership where
        parseJSON
          = withObject "VPCSecurityGroupMembership"
              (\ x ->
                 VPCSecurityGroupMembership' <$>
                   (x .:? "Status") <*> (x .:? "VpcSecurityGroupId"))

instance Hashable VPCSecurityGroupMembership where

instance NFData VPCSecurityGroupMembership where
