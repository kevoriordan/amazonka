{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.StorageClass
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glacier.Types.StorageClass (
  StorageClass (
    ..
    , ReducedRedundancy
    , Standard
    , StandardIA
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StorageClass = StorageClass' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern ReducedRedundancy :: StorageClass
pattern ReducedRedundancy = StorageClass' "REDUCED_REDUNDANCY"

pattern Standard :: StorageClass
pattern Standard = StorageClass' "STANDARD"

pattern StandardIA :: StorageClass
pattern StandardIA = StorageClass' "STANDARD_IA"

{-# COMPLETE
  ReducedRedundancy,
  Standard,
  StandardIA,
  StorageClass' #-}

instance FromText StorageClass where
    parser = (StorageClass' . mk) <$> takeText

instance ToText StorageClass where
    toText (StorageClass' ci) = original ci

-- | Represents an enum of /known/ $StorageClass.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StorageClass where
    toEnum i = case i of
        0 -> ReducedRedundancy
        1 -> Standard
        2 -> StandardIA
        _ -> (error . showText) $ "Unknown index for StorageClass: " <> toText i
    fromEnum x = case x of
        ReducedRedundancy -> 0
        Standard -> 1
        StandardIA -> 2
        StorageClass' name -> (error . showText) $ "Unknown StorageClass: " <> original name

-- | Represents the bounds of /known/ $StorageClass.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StorageClass where
    minBound = ReducedRedundancy
    maxBound = StandardIA

instance Hashable     StorageClass
instance NFData       StorageClass
instance ToByteString StorageClass
instance ToQuery      StorageClass
instance ToHeader     StorageClass

instance ToJSON StorageClass where
    toJSON = toJSONText

instance FromJSON StorageClass where
    parseJSON = parseJSONText "StorageClass"
