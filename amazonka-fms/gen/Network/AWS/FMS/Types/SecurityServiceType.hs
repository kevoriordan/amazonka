{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.SecurityServiceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.FMS.Types.SecurityServiceType (
  SecurityServiceType (
    ..
    , SecurityGroupsCommon
    , SecurityGroupsContentAudit
    , SecurityGroupsUsageAudit
    , ShieldAdvanced
    , WAFV2
    , Waf
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SecurityServiceType = SecurityServiceType' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern SecurityGroupsCommon :: SecurityServiceType
pattern SecurityGroupsCommon = SecurityServiceType' "SECURITY_GROUPS_COMMON"

pattern SecurityGroupsContentAudit :: SecurityServiceType
pattern SecurityGroupsContentAudit = SecurityServiceType' "SECURITY_GROUPS_CONTENT_AUDIT"

pattern SecurityGroupsUsageAudit :: SecurityServiceType
pattern SecurityGroupsUsageAudit = SecurityServiceType' "SECURITY_GROUPS_USAGE_AUDIT"

pattern ShieldAdvanced :: SecurityServiceType
pattern ShieldAdvanced = SecurityServiceType' "SHIELD_ADVANCED"

pattern WAFV2 :: SecurityServiceType
pattern WAFV2 = SecurityServiceType' "WAFV2"

pattern Waf :: SecurityServiceType
pattern Waf = SecurityServiceType' "WAF"

{-# COMPLETE
  SecurityGroupsCommon,
  SecurityGroupsContentAudit,
  SecurityGroupsUsageAudit,
  ShieldAdvanced,
  WAFV2,
  Waf,
  SecurityServiceType' #-}

instance FromText SecurityServiceType where
    parser = (SecurityServiceType' . mk) <$> takeText

instance ToText SecurityServiceType where
    toText (SecurityServiceType' ci) = original ci

-- | Represents an enum of /known/ $SecurityServiceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SecurityServiceType where
    toEnum i = case i of
        0 -> SecurityGroupsCommon
        1 -> SecurityGroupsContentAudit
        2 -> SecurityGroupsUsageAudit
        3 -> ShieldAdvanced
        4 -> WAFV2
        5 -> Waf
        _ -> (error . showText) $ "Unknown index for SecurityServiceType: " <> toText i
    fromEnum x = case x of
        SecurityGroupsCommon -> 0
        SecurityGroupsContentAudit -> 1
        SecurityGroupsUsageAudit -> 2
        ShieldAdvanced -> 3
        WAFV2 -> 4
        Waf -> 5
        SecurityServiceType' name -> (error . showText) $ "Unknown SecurityServiceType: " <> original name

-- | Represents the bounds of /known/ $SecurityServiceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SecurityServiceType where
    minBound = SecurityGroupsCommon
    maxBound = Waf

instance Hashable     SecurityServiceType
instance NFData       SecurityServiceType
instance ToByteString SecurityServiceType
instance ToQuery      SecurityServiceType
instance ToHeader     SecurityServiceType

instance ToJSON SecurityServiceType where
    toJSON = toJSONText

instance FromJSON SecurityServiceType where
    parseJSON = parseJSONText "SecurityServiceType"
