{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.FindingStatisticType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.FindingStatisticType (
  FindingStatisticType (
    ..
    , CountBySeverity
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data FindingStatisticType = FindingStatisticType' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern CountBySeverity :: FindingStatisticType
pattern CountBySeverity = FindingStatisticType' "COUNT_BY_SEVERITY"

{-# COMPLETE
  CountBySeverity,
  FindingStatisticType' #-}

instance FromText FindingStatisticType where
    parser = (FindingStatisticType' . mk) <$> takeText

instance ToText FindingStatisticType where
    toText (FindingStatisticType' ci) = original ci

-- | Represents an enum of /known/ $FindingStatisticType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FindingStatisticType where
    toEnum i = case i of
        0 -> CountBySeverity
        _ -> (error . showText) $ "Unknown index for FindingStatisticType: " <> toText i
    fromEnum x = case x of
        CountBySeverity -> 0
        FindingStatisticType' name -> (error . showText) $ "Unknown FindingStatisticType: " <> original name

-- | Represents the bounds of /known/ $FindingStatisticType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FindingStatisticType where
    minBound = CountBySeverity
    maxBound = CountBySeverity

instance Hashable     FindingStatisticType
instance NFData       FindingStatisticType
instance ToByteString FindingStatisticType
instance ToQuery      FindingStatisticType
instance ToHeader     FindingStatisticType

instance ToJSON FindingStatisticType where
    toJSON = toJSONText
