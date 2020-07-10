{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.EnablementTypeFilter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.EnablementTypeFilter (
  EnablementTypeFilter (
    ..
    , ETFEnabled
    , ETFPending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EnablementTypeFilter = EnablementTypeFilter' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern ETFEnabled :: EnablementTypeFilter
pattern ETFEnabled = EnablementTypeFilter' "ENABLED"

pattern ETFPending :: EnablementTypeFilter
pattern ETFPending = EnablementTypeFilter' "PENDING"

{-# COMPLETE
  ETFEnabled,
  ETFPending,
  EnablementTypeFilter' #-}

instance FromText EnablementTypeFilter where
    parser = (EnablementTypeFilter' . mk) <$> takeText

instance ToText EnablementTypeFilter where
    toText (EnablementTypeFilter' ci) = original ci

-- | Represents an enum of /known/ $EnablementTypeFilter.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EnablementTypeFilter where
    toEnum i = case i of
        0 -> ETFEnabled
        1 -> ETFPending
        _ -> (error . showText) $ "Unknown index for EnablementTypeFilter: " <> toText i
    fromEnum x = case x of
        ETFEnabled -> 0
        ETFPending -> 1
        EnablementTypeFilter' name -> (error . showText) $ "Unknown EnablementTypeFilter: " <> original name

-- | Represents the bounds of /known/ $EnablementTypeFilter.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EnablementTypeFilter where
    minBound = ETFEnabled
    maxBound = ETFPending

instance Hashable     EnablementTypeFilter
instance NFData       EnablementTypeFilter
instance ToByteString EnablementTypeFilter
instance ToQuery      EnablementTypeFilter
instance ToHeader     EnablementTypeFilter

instance ToJSON EnablementTypeFilter where
    toJSON = toJSONText
