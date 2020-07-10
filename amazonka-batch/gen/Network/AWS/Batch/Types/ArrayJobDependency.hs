{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ArrayJobDependency
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Batch.Types.ArrayJobDependency (
  ArrayJobDependency (
    ..
    , NToN
    , Sequential
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ArrayJobDependency = ArrayJobDependency' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern NToN :: ArrayJobDependency
pattern NToN = ArrayJobDependency' "N_TO_N"

pattern Sequential :: ArrayJobDependency
pattern Sequential = ArrayJobDependency' "SEQUENTIAL"

{-# COMPLETE
  NToN,
  Sequential,
  ArrayJobDependency' #-}

instance FromText ArrayJobDependency where
    parser = (ArrayJobDependency' . mk) <$> takeText

instance ToText ArrayJobDependency where
    toText (ArrayJobDependency' ci) = original ci

-- | Represents an enum of /known/ $ArrayJobDependency.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ArrayJobDependency where
    toEnum i = case i of
        0 -> NToN
        1 -> Sequential
        _ -> (error . showText) $ "Unknown index for ArrayJobDependency: " <> toText i
    fromEnum x = case x of
        NToN -> 0
        Sequential -> 1
        ArrayJobDependency' name -> (error . showText) $ "Unknown ArrayJobDependency: " <> original name

-- | Represents the bounds of /known/ $ArrayJobDependency.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ArrayJobDependency where
    minBound = NToN
    maxBound = Sequential

instance Hashable     ArrayJobDependency
instance NFData       ArrayJobDependency
instance ToByteString ArrayJobDependency
instance ToQuery      ArrayJobDependency
instance ToHeader     ArrayJobDependency

instance ToJSON ArrayJobDependency where
    toJSON = toJSONText

instance FromJSON ArrayJobDependency where
    parseJSON = parseJSONText "ArrayJobDependency"
