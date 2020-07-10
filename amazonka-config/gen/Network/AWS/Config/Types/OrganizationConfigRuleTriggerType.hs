{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationConfigRuleTriggerType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.OrganizationConfigRuleTriggerType (
  OrganizationConfigRuleTriggerType (
    ..
    , OCRTTConfigurationItemChangeNotification
    , OCRTTOversizedConfigurationItemChangeNotification
    , OCRTTScheduledNotification
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data OrganizationConfigRuleTriggerType = OrganizationConfigRuleTriggerType' (CI
                                                                               Text)
                                           deriving (Eq, Ord, Read, Show, Data,
                                                     Typeable, Generic)

pattern OCRTTConfigurationItemChangeNotification :: OrganizationConfigRuleTriggerType
pattern OCRTTConfigurationItemChangeNotification = OrganizationConfigRuleTriggerType' "ConfigurationItemChangeNotification"

pattern OCRTTOversizedConfigurationItemChangeNotification :: OrganizationConfigRuleTriggerType
pattern OCRTTOversizedConfigurationItemChangeNotification = OrganizationConfigRuleTriggerType' "OversizedConfigurationItemChangeNotification"

pattern OCRTTScheduledNotification :: OrganizationConfigRuleTriggerType
pattern OCRTTScheduledNotification = OrganizationConfigRuleTriggerType' "ScheduledNotification"

{-# COMPLETE
  OCRTTConfigurationItemChangeNotification,
  OCRTTOversizedConfigurationItemChangeNotification,
  OCRTTScheduledNotification,
  OrganizationConfigRuleTriggerType' #-}

instance FromText OrganizationConfigRuleTriggerType where
    parser = (OrganizationConfigRuleTriggerType' . mk) <$> takeText

instance ToText OrganizationConfigRuleTriggerType where
    toText (OrganizationConfigRuleTriggerType' ci) = original ci

-- | Represents an enum of /known/ $OrganizationConfigRuleTriggerType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OrganizationConfigRuleTriggerType where
    toEnum i = case i of
        0 -> OCRTTConfigurationItemChangeNotification
        1 -> OCRTTOversizedConfigurationItemChangeNotification
        2 -> OCRTTScheduledNotification
        _ -> (error . showText) $ "Unknown index for OrganizationConfigRuleTriggerType: " <> toText i
    fromEnum x = case x of
        OCRTTConfigurationItemChangeNotification -> 0
        OCRTTOversizedConfigurationItemChangeNotification -> 1
        OCRTTScheduledNotification -> 2
        OrganizationConfigRuleTriggerType' name -> (error . showText) $ "Unknown OrganizationConfigRuleTriggerType: " <> original name

-- | Represents the bounds of /known/ $OrganizationConfigRuleTriggerType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OrganizationConfigRuleTriggerType where
    minBound = OCRTTConfigurationItemChangeNotification
    maxBound = OCRTTScheduledNotification

instance Hashable     OrganizationConfigRuleTriggerType
instance NFData       OrganizationConfigRuleTriggerType
instance ToByteString OrganizationConfigRuleTriggerType
instance ToQuery      OrganizationConfigRuleTriggerType
instance ToHeader     OrganizationConfigRuleTriggerType

instance ToJSON OrganizationConfigRuleTriggerType where
    toJSON = toJSONText

instance FromJSON OrganizationConfigRuleTriggerType where
    parseJSON = parseJSONText "OrganizationConfigRuleTriggerType"
