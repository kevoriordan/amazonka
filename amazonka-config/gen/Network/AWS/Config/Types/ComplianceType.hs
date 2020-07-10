{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ComplianceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.ComplianceType (
  ComplianceType (
    ..
    , Compliant
    , InsufficientData
    , NonCompliant
    , NotApplicable
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ComplianceType = ComplianceType' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern Compliant :: ComplianceType
pattern Compliant = ComplianceType' "COMPLIANT"

pattern InsufficientData :: ComplianceType
pattern InsufficientData = ComplianceType' "INSUFFICIENT_DATA"

pattern NonCompliant :: ComplianceType
pattern NonCompliant = ComplianceType' "NON_COMPLIANT"

pattern NotApplicable :: ComplianceType
pattern NotApplicable = ComplianceType' "NOT_APPLICABLE"

{-# COMPLETE
  Compliant,
  InsufficientData,
  NonCompliant,
  NotApplicable,
  ComplianceType' #-}

instance FromText ComplianceType where
    parser = (ComplianceType' . mk) <$> takeText

instance ToText ComplianceType where
    toText (ComplianceType' ci) = original ci

-- | Represents an enum of /known/ $ComplianceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ComplianceType where
    toEnum i = case i of
        0 -> Compliant
        1 -> InsufficientData
        2 -> NonCompliant
        3 -> NotApplicable
        _ -> (error . showText) $ "Unknown index for ComplianceType: " <> toText i
    fromEnum x = case x of
        Compliant -> 0
        InsufficientData -> 1
        NonCompliant -> 2
        NotApplicable -> 3
        ComplianceType' name -> (error . showText) $ "Unknown ComplianceType: " <> original name

-- | Represents the bounds of /known/ $ComplianceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ComplianceType where
    minBound = Compliant
    maxBound = NotApplicable

instance Hashable     ComplianceType
instance NFData       ComplianceType
instance ToByteString ComplianceType
instance ToQuery      ComplianceType
instance ToHeader     ComplianceType

instance ToJSON ComplianceType where
    toJSON = toJSONText

instance FromJSON ComplianceType where
    parseJSON = parseJSONText "ComplianceType"
