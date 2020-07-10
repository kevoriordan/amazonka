{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigRuleComplianceSummaryGroupKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.ConfigRuleComplianceSummaryGroupKey (
  ConfigRuleComplianceSummaryGroupKey (
    ..
    , AWSRegion
    , AccountId
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ConfigRuleComplianceSummaryGroupKey = ConfigRuleComplianceSummaryGroupKey' (CI
                                                                                   Text)
                                             deriving (Eq, Ord, Read, Show,
                                                       Data, Typeable, Generic)

pattern AWSRegion :: ConfigRuleComplianceSummaryGroupKey
pattern AWSRegion = ConfigRuleComplianceSummaryGroupKey' "AWS_REGION"

pattern AccountId :: ConfigRuleComplianceSummaryGroupKey
pattern AccountId = ConfigRuleComplianceSummaryGroupKey' "ACCOUNT_ID"

{-# COMPLETE
  AWSRegion,
  AccountId,
  ConfigRuleComplianceSummaryGroupKey' #-}

instance FromText ConfigRuleComplianceSummaryGroupKey where
    parser = (ConfigRuleComplianceSummaryGroupKey' . mk) <$> takeText

instance ToText ConfigRuleComplianceSummaryGroupKey where
    toText (ConfigRuleComplianceSummaryGroupKey' ci) = original ci

-- | Represents an enum of /known/ $ConfigRuleComplianceSummaryGroupKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ConfigRuleComplianceSummaryGroupKey where
    toEnum i = case i of
        0 -> AWSRegion
        1 -> AccountId
        _ -> (error . showText) $ "Unknown index for ConfigRuleComplianceSummaryGroupKey: " <> toText i
    fromEnum x = case x of
        AWSRegion -> 0
        AccountId -> 1
        ConfigRuleComplianceSummaryGroupKey' name -> (error . showText) $ "Unknown ConfigRuleComplianceSummaryGroupKey: " <> original name

-- | Represents the bounds of /known/ $ConfigRuleComplianceSummaryGroupKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ConfigRuleComplianceSummaryGroupKey where
    minBound = AWSRegion
    maxBound = AccountId

instance Hashable     ConfigRuleComplianceSummaryGroupKey
instance NFData       ConfigRuleComplianceSummaryGroupKey
instance ToByteString ConfigRuleComplianceSummaryGroupKey
instance ToQuery      ConfigRuleComplianceSummaryGroupKey
instance ToHeader     ConfigRuleComplianceSummaryGroupKey

instance ToJSON ConfigRuleComplianceSummaryGroupKey where
    toJSON = toJSONText
