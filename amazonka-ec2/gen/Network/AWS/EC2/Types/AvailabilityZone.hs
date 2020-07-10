{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AvailabilityZone
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.AvailabilityZone where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AvailabilityZoneMessage
import Network.AWS.EC2.Types.AvailabilityZoneOptInStatus
import Network.AWS.EC2.Types.AvailabilityZoneState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an Availability Zone or Local Zone.
--
--
--
-- /See:/ 'availabilityZone' smart constructor.
data AvailabilityZone = AvailabilityZone'{_azState ::
                                          !(Maybe AvailabilityZoneState),
                                          _azRegionName :: !(Maybe Text),
                                          _azNetworkBorderGroup ::
                                          !(Maybe Text),
                                          _azZoneId :: !(Maybe Text),
                                          _azZoneName :: !(Maybe Text),
                                          _azOptInStatus ::
                                          !(Maybe AvailabilityZoneOptInStatus),
                                          _azMessages ::
                                          !(Maybe [AvailabilityZoneMessage]),
                                          _azGroupName :: !(Maybe Text)}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AvailabilityZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'azState' - The state of the Availability Zone or Local Zone.
--
-- * 'azRegionName' - The name of the Region.
--
-- * 'azNetworkBorderGroup' - The name of the location from which the address is advertised.
--
-- * 'azZoneId' - The ID of the Availability Zone or Local Zone.
--
-- * 'azZoneName' - The name of the Availability Zone or Local Zone.
--
-- * 'azOptInStatus' - For Availability Zones, this parameter always has the value of @opt-in-not-required@ . For Local Zones, this parameter is the opt in status. The possible values are @opted-in@ , and @not-opted-in@ .
--
-- * 'azMessages' - Any messages about the Availability Zone or Local Zone.
--
-- * 'azGroupName' - For Availability Zones, this parameter has the same value as the Region name. For Local Zones, the name of the associated group, for example @us-west-2-lax-1@ .
availabilityZone
    :: AvailabilityZone
availabilityZone
  = AvailabilityZone'{_azState = Nothing,
                      _azRegionName = Nothing,
                      _azNetworkBorderGroup = Nothing, _azZoneId = Nothing,
                      _azZoneName = Nothing, _azOptInStatus = Nothing,
                      _azMessages = Nothing, _azGroupName = Nothing}

-- | The state of the Availability Zone or Local Zone.
azState :: Lens' AvailabilityZone (Maybe AvailabilityZoneState)
azState = lens _azState (\ s a -> s{_azState = a})

-- | The name of the Region.
azRegionName :: Lens' AvailabilityZone (Maybe Text)
azRegionName = lens _azRegionName (\ s a -> s{_azRegionName = a})

-- | The name of the location from which the address is advertised.
azNetworkBorderGroup :: Lens' AvailabilityZone (Maybe Text)
azNetworkBorderGroup = lens _azNetworkBorderGroup (\ s a -> s{_azNetworkBorderGroup = a})

-- | The ID of the Availability Zone or Local Zone.
azZoneId :: Lens' AvailabilityZone (Maybe Text)
azZoneId = lens _azZoneId (\ s a -> s{_azZoneId = a})

-- | The name of the Availability Zone or Local Zone.
azZoneName :: Lens' AvailabilityZone (Maybe Text)
azZoneName = lens _azZoneName (\ s a -> s{_azZoneName = a})

-- | For Availability Zones, this parameter always has the value of @opt-in-not-required@ . For Local Zones, this parameter is the opt in status. The possible values are @opted-in@ , and @not-opted-in@ .
azOptInStatus :: Lens' AvailabilityZone (Maybe AvailabilityZoneOptInStatus)
azOptInStatus = lens _azOptInStatus (\ s a -> s{_azOptInStatus = a})

-- | Any messages about the Availability Zone or Local Zone.
azMessages :: Lens' AvailabilityZone [AvailabilityZoneMessage]
azMessages = lens _azMessages (\ s a -> s{_azMessages = a}) . _Default . _Coerce

-- | For Availability Zones, this parameter has the same value as the Region name. For Local Zones, the name of the associated group, for example @us-west-2-lax-1@ .
azGroupName :: Lens' AvailabilityZone (Maybe Text)
azGroupName = lens _azGroupName (\ s a -> s{_azGroupName = a})

instance FromXML AvailabilityZone where
        parseXML x
          = AvailabilityZone' <$>
              (x .@? "zoneState") <*> (x .@? "regionName") <*>
                (x .@? "networkBorderGroup")
                <*> (x .@? "zoneId")
                <*> (x .@? "zoneName")
                <*> (x .@? "optInStatus")
                <*>
                (x .@? "messageSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "groupName")

instance Hashable AvailabilityZone where

instance NFData AvailabilityZone where
