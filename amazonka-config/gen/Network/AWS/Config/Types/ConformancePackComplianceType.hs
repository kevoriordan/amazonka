{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConformancePackComplianceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.ConformancePackComplianceType (
  ConformancePackComplianceType (
    ..
    , CPCTCompliant
    , CPCTNonCompliant
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ConformancePackComplianceType = ConformancePackComplianceType' (CI
                                                                       Text)
                                       deriving (Eq, Ord, Read, Show, Data,
                                                 Typeable, Generic)

pattern CPCTCompliant :: ConformancePackComplianceType
pattern CPCTCompliant = ConformancePackComplianceType' "COMPLIANT"

pattern CPCTNonCompliant :: ConformancePackComplianceType
pattern CPCTNonCompliant = ConformancePackComplianceType' "NON_COMPLIANT"

{-# COMPLETE
  CPCTCompliant,
  CPCTNonCompliant,
  ConformancePackComplianceType' #-}

instance FromText ConformancePackComplianceType where
    parser = (ConformancePackComplianceType' . mk) <$> takeText

instance ToText ConformancePackComplianceType where
    toText (ConformancePackComplianceType' ci) = original ci

-- | Represents an enum of /known/ $ConformancePackComplianceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ConformancePackComplianceType where
    toEnum i = case i of
        0 -> CPCTCompliant
        1 -> CPCTNonCompliant
        _ -> (error . showText) $ "Unknown index for ConformancePackComplianceType: " <> toText i
    fromEnum x = case x of
        CPCTCompliant -> 0
        CPCTNonCompliant -> 1
        ConformancePackComplianceType' name -> (error . showText) $ "Unknown ConformancePackComplianceType: " <> original name

-- | Represents the bounds of /known/ $ConformancePackComplianceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ConformancePackComplianceType where
    minBound = CPCTCompliant
    maxBound = CPCTNonCompliant

instance Hashable     ConformancePackComplianceType
instance NFData       ConformancePackComplianceType
instance ToByteString ConformancePackComplianceType
instance ToQuery      ConformancePackComplianceType
instance ToHeader     ConformancePackComplianceType

instance ToJSON ConformancePackComplianceType where
    toJSON = toJSONText

instance FromJSON ConformancePackComplianceType where
    parseJSON = parseJSONText "ConformancePackComplianceType"
