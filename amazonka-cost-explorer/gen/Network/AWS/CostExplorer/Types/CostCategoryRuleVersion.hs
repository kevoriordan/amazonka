{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CostCategoryRuleVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.CostCategoryRuleVersion (
  CostCategoryRuleVersion (
    ..
    , CostCategoryExpression_V1
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The rule schema version in this particular Cost Category.
--
--
data CostCategoryRuleVersion = CostCategoryRuleVersion' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern CostCategoryExpression_V1 :: CostCategoryRuleVersion
pattern CostCategoryExpression_V1 = CostCategoryRuleVersion' "CostCategoryExpression.v1"

{-# COMPLETE
  CostCategoryExpression_V1,
  CostCategoryRuleVersion' #-}

instance FromText CostCategoryRuleVersion where
    parser = (CostCategoryRuleVersion' . mk) <$> takeText

instance ToText CostCategoryRuleVersion where
    toText (CostCategoryRuleVersion' ci) = original ci

-- | Represents an enum of /known/ $CostCategoryRuleVersion.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CostCategoryRuleVersion where
    toEnum i = case i of
        0 -> CostCategoryExpression_V1
        _ -> (error . showText) $ "Unknown index for CostCategoryRuleVersion: " <> toText i
    fromEnum x = case x of
        CostCategoryExpression_V1 -> 0
        CostCategoryRuleVersion' name -> (error . showText) $ "Unknown CostCategoryRuleVersion: " <> original name

-- | Represents the bounds of /known/ $CostCategoryRuleVersion.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CostCategoryRuleVersion where
    minBound = CostCategoryExpression_V1
    maxBound = CostCategoryExpression_V1

instance Hashable     CostCategoryRuleVersion
instance NFData       CostCategoryRuleVersion
instance ToByteString CostCategoryRuleVersion
instance ToQuery      CostCategoryRuleVersion
instance ToHeader     CostCategoryRuleVersion

instance ToJSON CostCategoryRuleVersion where
    toJSON = toJSONText

instance FromJSON CostCategoryRuleVersion where
    parseJSON = parseJSONText "CostCategoryRuleVersion"
