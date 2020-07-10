{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.RecurringChargeFrequency
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.RecurringChargeFrequency (
  RecurringChargeFrequency (
    ..
    , Monthly
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RecurringChargeFrequency = RecurringChargeFrequency' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern Monthly :: RecurringChargeFrequency
pattern Monthly = RecurringChargeFrequency' "MONTHLY"

{-# COMPLETE
  Monthly,
  RecurringChargeFrequency' #-}

instance FromText RecurringChargeFrequency where
    parser = (RecurringChargeFrequency' . mk) <$> takeText

instance ToText RecurringChargeFrequency where
    toText (RecurringChargeFrequency' ci) = original ci

-- | Represents an enum of /known/ $RecurringChargeFrequency.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RecurringChargeFrequency where
    toEnum i = case i of
        0 -> Monthly
        _ -> (error . showText) $ "Unknown index for RecurringChargeFrequency: " <> toText i
    fromEnum x = case x of
        Monthly -> 0
        RecurringChargeFrequency' name -> (error . showText) $ "Unknown RecurringChargeFrequency: " <> original name

-- | Represents the bounds of /known/ $RecurringChargeFrequency.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RecurringChargeFrequency where
    minBound = Monthly
    maxBound = Monthly

instance Hashable     RecurringChargeFrequency
instance NFData       RecurringChargeFrequency
instance ToByteString RecurringChargeFrequency
instance ToQuery      RecurringChargeFrequency
instance ToHeader     RecurringChargeFrequency

instance FromJSON RecurringChargeFrequency where
    parseJSON = parseJSONText "RecurringChargeFrequency"
