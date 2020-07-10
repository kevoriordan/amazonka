{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.MLModelType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MachineLearning.Types.MLModelType (
  MLModelType (
    ..
    , Binary
    , Multiclass
    , Regression
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MLModelType = MLModelType' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Binary :: MLModelType
pattern Binary = MLModelType' "BINARY"

pattern Multiclass :: MLModelType
pattern Multiclass = MLModelType' "MULTICLASS"

pattern Regression :: MLModelType
pattern Regression = MLModelType' "REGRESSION"

{-# COMPLETE
  Binary,
  Multiclass,
  Regression,
  MLModelType' #-}

instance FromText MLModelType where
    parser = (MLModelType' . mk) <$> takeText

instance ToText MLModelType where
    toText (MLModelType' ci) = original ci

-- | Represents an enum of /known/ $MLModelType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MLModelType where
    toEnum i = case i of
        0 -> Binary
        1 -> Multiclass
        2 -> Regression
        _ -> (error . showText) $ "Unknown index for MLModelType: " <> toText i
    fromEnum x = case x of
        Binary -> 0
        Multiclass -> 1
        Regression -> 2
        MLModelType' name -> (error . showText) $ "Unknown MLModelType: " <> original name

-- | Represents the bounds of /known/ $MLModelType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MLModelType where
    minBound = Binary
    maxBound = Regression

instance Hashable     MLModelType
instance NFData       MLModelType
instance ToByteString MLModelType
instance ToQuery      MLModelType
instance ToHeader     MLModelType

instance ToJSON MLModelType where
    toJSON = toJSONText

instance FromJSON MLModelType where
    parseJSON = parseJSONText "MLModelType"
