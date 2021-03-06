{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.Algorithm
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MachineLearning.Types.Algorithm (
  Algorithm (
    ..
    , SGD
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The function used to train an @MLModel@ . Training choices supported by Amazon ML include the following:
--
--
--     * @SGD@ - Stochastic Gradient Descent.    * @RandomForest@ - Random forest of decision trees.
--
data Algorithm = Algorithm' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern SGD :: Algorithm
pattern SGD = Algorithm' "sgd"

{-# COMPLETE
  SGD,
  Algorithm' #-}

instance FromText Algorithm where
    parser = (Algorithm' . mk) <$> takeText

instance ToText Algorithm where
    toText (Algorithm' ci) = original ci

-- | Represents an enum of /known/ $Algorithm.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Algorithm where
    toEnum i = case i of
        0 -> SGD
        _ -> (error . showText) $ "Unknown index for Algorithm: " <> toText i
    fromEnum x = case x of
        SGD -> 0
        Algorithm' name -> (error . showText) $ "Unknown Algorithm: " <> original name

-- | Represents the bounds of /known/ $Algorithm.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Algorithm where
    minBound = SGD
    maxBound = SGD

instance Hashable     Algorithm
instance NFData       Algorithm
instance ToByteString Algorithm
instance ToQuery      Algorithm
instance ToHeader     Algorithm

instance FromJSON Algorithm where
    parseJSON = parseJSONText "Algorithm"
