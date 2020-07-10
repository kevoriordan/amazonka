{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.MitigationActionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.MitigationActionType (
  MitigationActionType (
    ..
    , AddThingsToThingGroup
    , EnableIotLogging
    , PublishFindingToSNS
    , ReplaceDefaultPolicyVersion
    , UpdateCaCertificate
    , UpdateDeviceCertificate
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MitigationActionType = MitigationActionType' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern AddThingsToThingGroup :: MitigationActionType
pattern AddThingsToThingGroup = MitigationActionType' "ADD_THINGS_TO_THING_GROUP"

pattern EnableIotLogging :: MitigationActionType
pattern EnableIotLogging = MitigationActionType' "ENABLE_IOT_LOGGING"

pattern PublishFindingToSNS :: MitigationActionType
pattern PublishFindingToSNS = MitigationActionType' "PUBLISH_FINDING_TO_SNS"

pattern ReplaceDefaultPolicyVersion :: MitigationActionType
pattern ReplaceDefaultPolicyVersion = MitigationActionType' "REPLACE_DEFAULT_POLICY_VERSION"

pattern UpdateCaCertificate :: MitigationActionType
pattern UpdateCaCertificate = MitigationActionType' "UPDATE_CA_CERTIFICATE"

pattern UpdateDeviceCertificate :: MitigationActionType
pattern UpdateDeviceCertificate = MitigationActionType' "UPDATE_DEVICE_CERTIFICATE"

{-# COMPLETE
  AddThingsToThingGroup,
  EnableIotLogging,
  PublishFindingToSNS,
  ReplaceDefaultPolicyVersion,
  UpdateCaCertificate,
  UpdateDeviceCertificate,
  MitigationActionType' #-}

instance FromText MitigationActionType where
    parser = (MitigationActionType' . mk) <$> takeText

instance ToText MitigationActionType where
    toText (MitigationActionType' ci) = original ci

-- | Represents an enum of /known/ $MitigationActionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MitigationActionType where
    toEnum i = case i of
        0 -> AddThingsToThingGroup
        1 -> EnableIotLogging
        2 -> PublishFindingToSNS
        3 -> ReplaceDefaultPolicyVersion
        4 -> UpdateCaCertificate
        5 -> UpdateDeviceCertificate
        _ -> (error . showText) $ "Unknown index for MitigationActionType: " <> toText i
    fromEnum x = case x of
        AddThingsToThingGroup -> 0
        EnableIotLogging -> 1
        PublishFindingToSNS -> 2
        ReplaceDefaultPolicyVersion -> 3
        UpdateCaCertificate -> 4
        UpdateDeviceCertificate -> 5
        MitigationActionType' name -> (error . showText) $ "Unknown MitigationActionType: " <> original name

-- | Represents the bounds of /known/ $MitigationActionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MitigationActionType where
    minBound = AddThingsToThingGroup
    maxBound = UpdateDeviceCertificate

instance Hashable     MitigationActionType
instance NFData       MitigationActionType
instance ToByteString MitigationActionType
instance ToQuery      MitigationActionType
instance ToHeader     MitigationActionType

instance ToJSON MitigationActionType where
    toJSON = toJSONText

instance FromJSON MitigationActionType where
    parseJSON = parseJSONText "MitigationActionType"
