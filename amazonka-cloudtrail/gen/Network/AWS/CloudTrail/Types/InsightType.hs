{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.InsightType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudTrail.Types.InsightType (
  InsightType (
    ..
    , APICallRateInsight
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data InsightType = InsightType' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern APICallRateInsight :: InsightType
pattern APICallRateInsight = InsightType' "ApiCallRateInsight"

{-# COMPLETE
  APICallRateInsight,
  InsightType' #-}

instance FromText InsightType where
    parser = (InsightType' . mk) <$> takeText

instance ToText InsightType where
    toText (InsightType' ci) = original ci

-- | Represents an enum of /known/ $InsightType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InsightType where
    toEnum i = case i of
        0 -> APICallRateInsight
        _ -> (error . showText) $ "Unknown index for InsightType: " <> toText i
    fromEnum x = case x of
        APICallRateInsight -> 0
        InsightType' name -> (error . showText) $ "Unknown InsightType: " <> original name

-- | Represents the bounds of /known/ $InsightType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InsightType where
    minBound = APICallRateInsight
    maxBound = APICallRateInsight

instance Hashable     InsightType
instance NFData       InsightType
instance ToByteString InsightType
instance ToQuery      InsightType
instance ToHeader     InsightType

instance ToJSON InsightType where
    toJSON = toJSONText

instance FromJSON InsightType where
    parseJSON = parseJSONText "InsightType"
