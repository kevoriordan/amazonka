{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.Product (
    module Network.AWS.AlexaBusiness.Types.AddressBook,
    module Network.AWS.AlexaBusiness.Types.AddressBookData,
    module Network.AWS.AlexaBusiness.Types.Audio,
    module Network.AWS.AlexaBusiness.Types.BusinessReport,
    module Network.AWS.AlexaBusiness.Types.BusinessReportContentRange,
    module Network.AWS.AlexaBusiness.Types.BusinessReportRecurrence,
    module Network.AWS.AlexaBusiness.Types.BusinessReportS3Location,
    module Network.AWS.AlexaBusiness.Types.BusinessReportSchedule,
    module Network.AWS.AlexaBusiness.Types.Category,
    module Network.AWS.AlexaBusiness.Types.ConferencePreference,
    module Network.AWS.AlexaBusiness.Types.ConferenceProvider,
    module Network.AWS.AlexaBusiness.Types.Contact,
    module Network.AWS.AlexaBusiness.Types.ContactData,
    module Network.AWS.AlexaBusiness.Types.Content,
    module Network.AWS.AlexaBusiness.Types.CreateEndOfMeetingReminder,
    module Network.AWS.AlexaBusiness.Types.CreateInstantBooking,
    module Network.AWS.AlexaBusiness.Types.CreateMeetingRoomConfiguration,
    module Network.AWS.AlexaBusiness.Types.CreateRequireCheckIn,
    module Network.AWS.AlexaBusiness.Types.DeveloperInfo,
    module Network.AWS.AlexaBusiness.Types.Device,
    module Network.AWS.AlexaBusiness.Types.DeviceData,
    module Network.AWS.AlexaBusiness.Types.DeviceEvent,
    module Network.AWS.AlexaBusiness.Types.DeviceNetworkProfileInfo,
    module Network.AWS.AlexaBusiness.Types.DeviceStatusDetail,
    module Network.AWS.AlexaBusiness.Types.DeviceStatusInfo,
    module Network.AWS.AlexaBusiness.Types.EndOfMeetingReminder,
    module Network.AWS.AlexaBusiness.Types.Filter,
    module Network.AWS.AlexaBusiness.Types.Gateway,
    module Network.AWS.AlexaBusiness.Types.GatewayGroup,
    module Network.AWS.AlexaBusiness.Types.GatewayGroupSummary,
    module Network.AWS.AlexaBusiness.Types.GatewaySummary,
    module Network.AWS.AlexaBusiness.Types.IPDialIn,
    module Network.AWS.AlexaBusiness.Types.InstantBooking,
    module Network.AWS.AlexaBusiness.Types.MeetingRoomConfiguration,
    module Network.AWS.AlexaBusiness.Types.MeetingSetting,
    module Network.AWS.AlexaBusiness.Types.NetworkProfile,
    module Network.AWS.AlexaBusiness.Types.NetworkProfileData,
    module Network.AWS.AlexaBusiness.Types.PSTNDialIn,
    module Network.AWS.AlexaBusiness.Types.PhoneNumber,
    module Network.AWS.AlexaBusiness.Types.Profile,
    module Network.AWS.AlexaBusiness.Types.ProfileData,
    module Network.AWS.AlexaBusiness.Types.RequireCheckIn,
    module Network.AWS.AlexaBusiness.Types.Room,
    module Network.AWS.AlexaBusiness.Types.RoomData,
    module Network.AWS.AlexaBusiness.Types.RoomSkillParameter,
    module Network.AWS.AlexaBusiness.Types.SipAddress,
    module Network.AWS.AlexaBusiness.Types.SkillDetails,
    module Network.AWS.AlexaBusiness.Types.SkillGroup,
    module Network.AWS.AlexaBusiness.Types.SkillGroupData,
    module Network.AWS.AlexaBusiness.Types.SkillSummary,
    module Network.AWS.AlexaBusiness.Types.SkillsStoreSkill,
    module Network.AWS.AlexaBusiness.Types.SmartHomeAppliance,
    module Network.AWS.AlexaBusiness.Types.Sort,
    module Network.AWS.AlexaBusiness.Types.Ssml,
    module Network.AWS.AlexaBusiness.Types.Tag,
    module Network.AWS.AlexaBusiness.Types.TextMessage,
    module Network.AWS.AlexaBusiness.Types.UpdateEndOfMeetingReminder,
    module Network.AWS.AlexaBusiness.Types.UpdateInstantBooking,
    module Network.AWS.AlexaBusiness.Types.UpdateMeetingRoomConfiguration,
    module Network.AWS.AlexaBusiness.Types.UpdateRequireCheckIn,
    module Network.AWS.AlexaBusiness.Types.UserData
  ) where

import Network.AWS.AlexaBusiness.Types.AddressBook
import Network.AWS.AlexaBusiness.Types.AddressBookData
import Network.AWS.AlexaBusiness.Types.Audio
import Network.AWS.AlexaBusiness.Types.BusinessReport
import Network.AWS.AlexaBusiness.Types.BusinessReportContentRange
import Network.AWS.AlexaBusiness.Types.BusinessReportRecurrence
import Network.AWS.AlexaBusiness.Types.BusinessReportS3Location
import Network.AWS.AlexaBusiness.Types.BusinessReportSchedule
import Network.AWS.AlexaBusiness.Types.Category
import Network.AWS.AlexaBusiness.Types.ConferencePreference
import Network.AWS.AlexaBusiness.Types.ConferenceProvider
import Network.AWS.AlexaBusiness.Types.Contact
import Network.AWS.AlexaBusiness.Types.ContactData
import Network.AWS.AlexaBusiness.Types.Content
import Network.AWS.AlexaBusiness.Types.CreateEndOfMeetingReminder
import Network.AWS.AlexaBusiness.Types.CreateInstantBooking
import Network.AWS.AlexaBusiness.Types.CreateMeetingRoomConfiguration
import Network.AWS.AlexaBusiness.Types.CreateRequireCheckIn
import Network.AWS.AlexaBusiness.Types.DeveloperInfo
import Network.AWS.AlexaBusiness.Types.Device
import Network.AWS.AlexaBusiness.Types.DeviceData
import Network.AWS.AlexaBusiness.Types.DeviceEvent
import Network.AWS.AlexaBusiness.Types.DeviceNetworkProfileInfo
import Network.AWS.AlexaBusiness.Types.DeviceStatusDetail
import Network.AWS.AlexaBusiness.Types.DeviceStatusInfo
import Network.AWS.AlexaBusiness.Types.EndOfMeetingReminder
import Network.AWS.AlexaBusiness.Types.Filter
import Network.AWS.AlexaBusiness.Types.Gateway
import Network.AWS.AlexaBusiness.Types.GatewayGroup
import Network.AWS.AlexaBusiness.Types.GatewayGroupSummary
import Network.AWS.AlexaBusiness.Types.GatewaySummary
import Network.AWS.AlexaBusiness.Types.IPDialIn
import Network.AWS.AlexaBusiness.Types.InstantBooking
import Network.AWS.AlexaBusiness.Types.MeetingRoomConfiguration
import Network.AWS.AlexaBusiness.Types.MeetingSetting
import Network.AWS.AlexaBusiness.Types.NetworkProfile
import Network.AWS.AlexaBusiness.Types.NetworkProfileData
import Network.AWS.AlexaBusiness.Types.PSTNDialIn
import Network.AWS.AlexaBusiness.Types.PhoneNumber
import Network.AWS.AlexaBusiness.Types.Profile
import Network.AWS.AlexaBusiness.Types.ProfileData
import Network.AWS.AlexaBusiness.Types.RequireCheckIn
import Network.AWS.AlexaBusiness.Types.Room
import Network.AWS.AlexaBusiness.Types.RoomData
import Network.AWS.AlexaBusiness.Types.RoomSkillParameter
import Network.AWS.AlexaBusiness.Types.SipAddress
import Network.AWS.AlexaBusiness.Types.SkillDetails
import Network.AWS.AlexaBusiness.Types.SkillGroup
import Network.AWS.AlexaBusiness.Types.SkillGroupData
import Network.AWS.AlexaBusiness.Types.SkillSummary
import Network.AWS.AlexaBusiness.Types.SkillsStoreSkill
import Network.AWS.AlexaBusiness.Types.SmartHomeAppliance
import Network.AWS.AlexaBusiness.Types.Sort
import Network.AWS.AlexaBusiness.Types.Ssml
import Network.AWS.AlexaBusiness.Types.Tag
import Network.AWS.AlexaBusiness.Types.TextMessage
import Network.AWS.AlexaBusiness.Types.UpdateEndOfMeetingReminder
import Network.AWS.AlexaBusiness.Types.UpdateInstantBooking
import Network.AWS.AlexaBusiness.Types.UpdateMeetingRoomConfiguration
import Network.AWS.AlexaBusiness.Types.UpdateRequireCheckIn
import Network.AWS.AlexaBusiness.Types.UserData
import Network.AWS.Lens
import Network.AWS.Prelude
