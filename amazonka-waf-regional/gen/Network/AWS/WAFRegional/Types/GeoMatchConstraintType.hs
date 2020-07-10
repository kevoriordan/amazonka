{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.GeoMatchConstraintType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAFRegional.Types.GeoMatchConstraintType (
  GeoMatchConstraintType (
    ..
    , Country
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data GeoMatchConstraintType = GeoMatchConstraintType' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern Country :: GeoMatchConstraintType
pattern Country = GeoMatchConstraintType' "Country"

{-# COMPLETE
  Country,
  GeoMatchConstraintType' #-}

instance FromText GeoMatchConstraintType where
    parser = (GeoMatchConstraintType' . mk) <$> takeText

instance ToText GeoMatchConstraintType where
    toText (GeoMatchConstraintType' ci) = original ci

-- | Represents an enum of /known/ $GeoMatchConstraintType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum GeoMatchConstraintType where
    toEnum i = case i of
        0 -> Country
        _ -> (error . showText) $ "Unknown index for GeoMatchConstraintType: " <> toText i
    fromEnum x = case x of
        Country -> 0
        GeoMatchConstraintType' name -> (error . showText) $ "Unknown GeoMatchConstraintType: " <> original name

-- | Represents the bounds of /known/ $GeoMatchConstraintType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded GeoMatchConstraintType where
    minBound = Country
    maxBound = Country

instance Hashable     GeoMatchConstraintType
instance NFData       GeoMatchConstraintType
instance ToByteString GeoMatchConstraintType
instance ToQuery      GeoMatchConstraintType
instance ToHeader     GeoMatchConstraintType

instance ToJSON GeoMatchConstraintType where
    toJSON = toJSONText

instance FromJSON GeoMatchConstraintType where
    parseJSON = parseJSONText "GeoMatchConstraintType"
