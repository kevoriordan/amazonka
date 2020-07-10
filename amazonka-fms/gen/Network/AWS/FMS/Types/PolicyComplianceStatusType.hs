{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.PolicyComplianceStatusType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.FMS.Types.PolicyComplianceStatusType (
  PolicyComplianceStatusType (
    ..
    , Compliant
    , NonCompliant
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PolicyComplianceStatusType = PolicyComplianceStatusType' (CI
                                                                 Text)
                                    deriving (Eq, Ord, Read, Show, Data,
                                              Typeable, Generic)

pattern Compliant :: PolicyComplianceStatusType
pattern Compliant = PolicyComplianceStatusType' "COMPLIANT"

pattern NonCompliant :: PolicyComplianceStatusType
pattern NonCompliant = PolicyComplianceStatusType' "NON_COMPLIANT"

{-# COMPLETE
  Compliant,
  NonCompliant,
  PolicyComplianceStatusType' #-}

instance FromText PolicyComplianceStatusType where
    parser = (PolicyComplianceStatusType' . mk) <$> takeText

instance ToText PolicyComplianceStatusType where
    toText (PolicyComplianceStatusType' ci) = original ci

-- | Represents an enum of /known/ $PolicyComplianceStatusType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PolicyComplianceStatusType where
    toEnum i = case i of
        0 -> Compliant
        1 -> NonCompliant
        _ -> (error . showText) $ "Unknown index for PolicyComplianceStatusType: " <> toText i
    fromEnum x = case x of
        Compliant -> 0
        NonCompliant -> 1
        PolicyComplianceStatusType' name -> (error . showText) $ "Unknown PolicyComplianceStatusType: " <> original name

-- | Represents the bounds of /known/ $PolicyComplianceStatusType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PolicyComplianceStatusType where
    minBound = Compliant
    maxBound = NonCompliant

instance Hashable     PolicyComplianceStatusType
instance NFData       PolicyComplianceStatusType
instance ToByteString PolicyComplianceStatusType
instance ToQuery      PolicyComplianceStatusType
instance ToHeader     PolicyComplianceStatusType

instance FromJSON PolicyComplianceStatusType where
    parseJSON = parseJSONText "PolicyComplianceStatusType"
