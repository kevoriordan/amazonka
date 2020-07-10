{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.EndOfMeetingReminderType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.EndOfMeetingReminderType (
  EndOfMeetingReminderType (
    ..
    , EOMRTAnnouncementTimeCheck
    , EOMRTAnnouncementVariableTimeLeft
    , EOMRTChime
    , EOMRTKnock
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EndOfMeetingReminderType = EndOfMeetingReminderType' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern EOMRTAnnouncementTimeCheck :: EndOfMeetingReminderType
pattern EOMRTAnnouncementTimeCheck = EndOfMeetingReminderType' "ANNOUNCEMENT_TIME_CHECK"

pattern EOMRTAnnouncementVariableTimeLeft :: EndOfMeetingReminderType
pattern EOMRTAnnouncementVariableTimeLeft = EndOfMeetingReminderType' "ANNOUNCEMENT_VARIABLE_TIME_LEFT"

pattern EOMRTChime :: EndOfMeetingReminderType
pattern EOMRTChime = EndOfMeetingReminderType' "CHIME"

pattern EOMRTKnock :: EndOfMeetingReminderType
pattern EOMRTKnock = EndOfMeetingReminderType' "KNOCK"

{-# COMPLETE
  EOMRTAnnouncementTimeCheck,
  EOMRTAnnouncementVariableTimeLeft,
  EOMRTChime,
  EOMRTKnock,
  EndOfMeetingReminderType' #-}

instance FromText EndOfMeetingReminderType where
    parser = (EndOfMeetingReminderType' . mk) <$> takeText

instance ToText EndOfMeetingReminderType where
    toText (EndOfMeetingReminderType' ci) = original ci

-- | Represents an enum of /known/ $EndOfMeetingReminderType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EndOfMeetingReminderType where
    toEnum i = case i of
        0 -> EOMRTAnnouncementTimeCheck
        1 -> EOMRTAnnouncementVariableTimeLeft
        2 -> EOMRTChime
        3 -> EOMRTKnock
        _ -> (error . showText) $ "Unknown index for EndOfMeetingReminderType: " <> toText i
    fromEnum x = case x of
        EOMRTAnnouncementTimeCheck -> 0
        EOMRTAnnouncementVariableTimeLeft -> 1
        EOMRTChime -> 2
        EOMRTKnock -> 3
        EndOfMeetingReminderType' name -> (error . showText) $ "Unknown EndOfMeetingReminderType: " <> original name

-- | Represents the bounds of /known/ $EndOfMeetingReminderType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EndOfMeetingReminderType where
    minBound = EOMRTAnnouncementTimeCheck
    maxBound = EOMRTKnock

instance Hashable     EndOfMeetingReminderType
instance NFData       EndOfMeetingReminderType
instance ToByteString EndOfMeetingReminderType
instance ToQuery      EndOfMeetingReminderType
instance ToHeader     EndOfMeetingReminderType

instance ToJSON EndOfMeetingReminderType where
    toJSON = toJSONText

instance FromJSON EndOfMeetingReminderType where
    parseJSON = parseJSONText "EndOfMeetingReminderType"
