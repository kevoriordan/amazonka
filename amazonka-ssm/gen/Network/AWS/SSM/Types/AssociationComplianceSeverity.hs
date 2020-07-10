{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationComplianceSeverity
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.AssociationComplianceSeverity (
  AssociationComplianceSeverity (
    ..
    , ACSCritical
    , ACSHigh
    , ACSLow
    , ACSMedium
    , ACSUnspecified
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AssociationComplianceSeverity = AssociationComplianceSeverity' (CI
                                                                       Text)
                                       deriving (Eq, Ord, Read, Show, Data,
                                                 Typeable, Generic)

pattern ACSCritical :: AssociationComplianceSeverity
pattern ACSCritical = AssociationComplianceSeverity' "CRITICAL"

pattern ACSHigh :: AssociationComplianceSeverity
pattern ACSHigh = AssociationComplianceSeverity' "HIGH"

pattern ACSLow :: AssociationComplianceSeverity
pattern ACSLow = AssociationComplianceSeverity' "LOW"

pattern ACSMedium :: AssociationComplianceSeverity
pattern ACSMedium = AssociationComplianceSeverity' "MEDIUM"

pattern ACSUnspecified :: AssociationComplianceSeverity
pattern ACSUnspecified = AssociationComplianceSeverity' "UNSPECIFIED"

{-# COMPLETE
  ACSCritical,
  ACSHigh,
  ACSLow,
  ACSMedium,
  ACSUnspecified,
  AssociationComplianceSeverity' #-}

instance FromText AssociationComplianceSeverity where
    parser = (AssociationComplianceSeverity' . mk) <$> takeText

instance ToText AssociationComplianceSeverity where
    toText (AssociationComplianceSeverity' ci) = original ci

-- | Represents an enum of /known/ $AssociationComplianceSeverity.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AssociationComplianceSeverity where
    toEnum i = case i of
        0 -> ACSCritical
        1 -> ACSHigh
        2 -> ACSLow
        3 -> ACSMedium
        4 -> ACSUnspecified
        _ -> (error . showText) $ "Unknown index for AssociationComplianceSeverity: " <> toText i
    fromEnum x = case x of
        ACSCritical -> 0
        ACSHigh -> 1
        ACSLow -> 2
        ACSMedium -> 3
        ACSUnspecified -> 4
        AssociationComplianceSeverity' name -> (error . showText) $ "Unknown AssociationComplianceSeverity: " <> original name

-- | Represents the bounds of /known/ $AssociationComplianceSeverity.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AssociationComplianceSeverity where
    minBound = ACSCritical
    maxBound = ACSUnspecified

instance Hashable     AssociationComplianceSeverity
instance NFData       AssociationComplianceSeverity
instance ToByteString AssociationComplianceSeverity
instance ToQuery      AssociationComplianceSeverity
instance ToHeader     AssociationComplianceSeverity

instance ToJSON AssociationComplianceSeverity where
    toJSON = toJSONText

instance FromJSON AssociationComplianceSeverity where
    parseJSON = parseJSONText "AssociationComplianceSeverity"
