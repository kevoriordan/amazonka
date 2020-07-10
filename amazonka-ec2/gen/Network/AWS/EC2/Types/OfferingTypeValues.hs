{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.OfferingTypeValues
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.OfferingTypeValues (
  OfferingTypeValues (
    ..
    , AllUpfront
    , HeavyUtilization
    , LightUtilization
    , MediumUtilization
    , NoUpfront
    , PartialUpfront
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data OfferingTypeValues = OfferingTypeValues' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern AllUpfront :: OfferingTypeValues
pattern AllUpfront = OfferingTypeValues' "All Upfront"

pattern HeavyUtilization :: OfferingTypeValues
pattern HeavyUtilization = OfferingTypeValues' "Heavy Utilization"

pattern LightUtilization :: OfferingTypeValues
pattern LightUtilization = OfferingTypeValues' "Light Utilization"

pattern MediumUtilization :: OfferingTypeValues
pattern MediumUtilization = OfferingTypeValues' "Medium Utilization"

pattern NoUpfront :: OfferingTypeValues
pattern NoUpfront = OfferingTypeValues' "No Upfront"

pattern PartialUpfront :: OfferingTypeValues
pattern PartialUpfront = OfferingTypeValues' "Partial Upfront"

{-# COMPLETE
  AllUpfront,
  HeavyUtilization,
  LightUtilization,
  MediumUtilization,
  NoUpfront,
  PartialUpfront,
  OfferingTypeValues' #-}

instance FromText OfferingTypeValues where
    parser = (OfferingTypeValues' . mk) <$> takeText

instance ToText OfferingTypeValues where
    toText (OfferingTypeValues' ci) = original ci

-- | Represents an enum of /known/ $OfferingTypeValues.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OfferingTypeValues where
    toEnum i = case i of
        0 -> AllUpfront
        1 -> HeavyUtilization
        2 -> LightUtilization
        3 -> MediumUtilization
        4 -> NoUpfront
        5 -> PartialUpfront
        _ -> (error . showText) $ "Unknown index for OfferingTypeValues: " <> toText i
    fromEnum x = case x of
        AllUpfront -> 0
        HeavyUtilization -> 1
        LightUtilization -> 2
        MediumUtilization -> 3
        NoUpfront -> 4
        PartialUpfront -> 5
        OfferingTypeValues' name -> (error . showText) $ "Unknown OfferingTypeValues: " <> original name

-- | Represents the bounds of /known/ $OfferingTypeValues.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OfferingTypeValues where
    minBound = AllUpfront
    maxBound = PartialUpfront

instance Hashable     OfferingTypeValues
instance NFData       OfferingTypeValues
instance ToByteString OfferingTypeValues
instance ToQuery      OfferingTypeValues
instance ToHeader     OfferingTypeValues

instance FromXML OfferingTypeValues where
    parseXML = parseXMLText "OfferingTypeValues"
