{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ComplianceSeverity
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.ComplianceSeverity (
  ComplianceSeverity (
    ..
    , CSCritical
    , CSHigh
    , CSInformational
    , CSLow
    , CSMedium
    , CSUnspecified
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ComplianceSeverity = ComplianceSeverity' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern CSCritical :: ComplianceSeverity
pattern CSCritical = ComplianceSeverity' "CRITICAL"

pattern CSHigh :: ComplianceSeverity
pattern CSHigh = ComplianceSeverity' "HIGH"

pattern CSInformational :: ComplianceSeverity
pattern CSInformational = ComplianceSeverity' "INFORMATIONAL"

pattern CSLow :: ComplianceSeverity
pattern CSLow = ComplianceSeverity' "LOW"

pattern CSMedium :: ComplianceSeverity
pattern CSMedium = ComplianceSeverity' "MEDIUM"

pattern CSUnspecified :: ComplianceSeverity
pattern CSUnspecified = ComplianceSeverity' "UNSPECIFIED"

{-# COMPLETE
  CSCritical,
  CSHigh,
  CSInformational,
  CSLow,
  CSMedium,
  CSUnspecified,
  ComplianceSeverity' #-}

instance FromText ComplianceSeverity where
    parser = (ComplianceSeverity' . mk) <$> takeText

instance ToText ComplianceSeverity where
    toText (ComplianceSeverity' ci) = original ci

-- | Represents an enum of /known/ $ComplianceSeverity.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ComplianceSeverity where
    toEnum i = case i of
        0 -> CSCritical
        1 -> CSHigh
        2 -> CSInformational
        3 -> CSLow
        4 -> CSMedium
        5 -> CSUnspecified
        _ -> (error . showText) $ "Unknown index for ComplianceSeverity: " <> toText i
    fromEnum x = case x of
        CSCritical -> 0
        CSHigh -> 1
        CSInformational -> 2
        CSLow -> 3
        CSMedium -> 4
        CSUnspecified -> 5
        ComplianceSeverity' name -> (error . showText) $ "Unknown ComplianceSeverity: " <> original name

-- | Represents the bounds of /known/ $ComplianceSeverity.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ComplianceSeverity where
    minBound = CSCritical
    maxBound = CSUnspecified

instance Hashable     ComplianceSeverity
instance NFData       ComplianceSeverity
instance ToByteString ComplianceSeverity
instance ToQuery      ComplianceSeverity
instance ToHeader     ComplianceSeverity

instance ToJSON ComplianceSeverity where
    toJSON = toJSONText

instance FromJSON ComplianceSeverity where
    parseJSON = parseJSONText "ComplianceSeverity"
