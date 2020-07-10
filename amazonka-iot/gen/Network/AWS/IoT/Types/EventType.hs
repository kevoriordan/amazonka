{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.EventType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.EventType (
  EventType (
    ..
    , CaCertificate
    , Certificate
    , Job
    , JobExecution
    , Policy
    , Thing
    , ThingGroup
    , ThingGroupHierarchy
    , ThingGroupMembership
    , ThingType
    , ThingTypeAssociation
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EventType = EventType' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern CaCertificate :: EventType
pattern CaCertificate = EventType' "CA_CERTIFICATE"

pattern Certificate :: EventType
pattern Certificate = EventType' "CERTIFICATE"

pattern Job :: EventType
pattern Job = EventType' "JOB"

pattern JobExecution :: EventType
pattern JobExecution = EventType' "JOB_EXECUTION"

pattern Policy :: EventType
pattern Policy = EventType' "POLICY"

pattern Thing :: EventType
pattern Thing = EventType' "THING"

pattern ThingGroup :: EventType
pattern ThingGroup = EventType' "THING_GROUP"

pattern ThingGroupHierarchy :: EventType
pattern ThingGroupHierarchy = EventType' "THING_GROUP_HIERARCHY"

pattern ThingGroupMembership :: EventType
pattern ThingGroupMembership = EventType' "THING_GROUP_MEMBERSHIP"

pattern ThingType :: EventType
pattern ThingType = EventType' "THING_TYPE"

pattern ThingTypeAssociation :: EventType
pattern ThingTypeAssociation = EventType' "THING_TYPE_ASSOCIATION"

{-# COMPLETE
  CaCertificate,
  Certificate,
  Job,
  JobExecution,
  Policy,
  Thing,
  ThingGroup,
  ThingGroupHierarchy,
  ThingGroupMembership,
  ThingType,
  ThingTypeAssociation,
  EventType' #-}

instance FromText EventType where
    parser = (EventType' . mk) <$> takeText

instance ToText EventType where
    toText (EventType' ci) = original ci

-- | Represents an enum of /known/ $EventType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EventType where
    toEnum i = case i of
        0 -> CaCertificate
        1 -> Certificate
        2 -> Job
        3 -> JobExecution
        4 -> Policy
        5 -> Thing
        6 -> ThingGroup
        7 -> ThingGroupHierarchy
        8 -> ThingGroupMembership
        9 -> ThingType
        10 -> ThingTypeAssociation
        _ -> (error . showText) $ "Unknown index for EventType: " <> toText i
    fromEnum x = case x of
        CaCertificate -> 0
        Certificate -> 1
        Job -> 2
        JobExecution -> 3
        Policy -> 4
        Thing -> 5
        ThingGroup -> 6
        ThingGroupHierarchy -> 7
        ThingGroupMembership -> 8
        ThingType -> 9
        ThingTypeAssociation -> 10
        EventType' name -> (error . showText) $ "Unknown EventType: " <> original name

-- | Represents the bounds of /known/ $EventType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EventType where
    minBound = CaCertificate
    maxBound = ThingTypeAssociation

instance Hashable     EventType
instance NFData       EventType
instance ToByteString EventType
instance ToQuery      EventType
instance ToHeader     EventType

instance ToJSON EventType where
    toJSON = toJSONText

instance FromJSON EventType where
    parseJSON = parseJSONText "EventType"
