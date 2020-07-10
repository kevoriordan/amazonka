{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ModelSortKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ModelSortKey (
  ModelSortKey (
    ..
    , MSKCreationTime
    , MSKName
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ModelSortKey = ModelSortKey' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern MSKCreationTime :: ModelSortKey
pattern MSKCreationTime = ModelSortKey' "CreationTime"

pattern MSKName :: ModelSortKey
pattern MSKName = ModelSortKey' "Name"

{-# COMPLETE
  MSKCreationTime,
  MSKName,
  ModelSortKey' #-}

instance FromText ModelSortKey where
    parser = (ModelSortKey' . mk) <$> takeText

instance ToText ModelSortKey where
    toText (ModelSortKey' ci) = original ci

-- | Represents an enum of /known/ $ModelSortKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ModelSortKey where
    toEnum i = case i of
        0 -> MSKCreationTime
        1 -> MSKName
        _ -> (error . showText) $ "Unknown index for ModelSortKey: " <> toText i
    fromEnum x = case x of
        MSKCreationTime -> 0
        MSKName -> 1
        ModelSortKey' name -> (error . showText) $ "Unknown ModelSortKey: " <> original name

-- | Represents the bounds of /known/ $ModelSortKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ModelSortKey where
    minBound = MSKCreationTime
    maxBound = MSKName

instance Hashable     ModelSortKey
instance NFData       ModelSortKey
instance ToByteString ModelSortKey
instance ToQuery      ModelSortKey
instance ToHeader     ModelSortKey

instance ToJSON ModelSortKey where
    toJSON = toJSONText
