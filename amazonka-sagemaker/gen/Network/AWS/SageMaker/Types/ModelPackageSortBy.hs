{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ModelPackageSortBy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ModelPackageSortBy (
  ModelPackageSortBy (
    ..
    , CreationTime
    , Name
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ModelPackageSortBy = ModelPackageSortBy' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern CreationTime :: ModelPackageSortBy
pattern CreationTime = ModelPackageSortBy' "CreationTime"

pattern Name :: ModelPackageSortBy
pattern Name = ModelPackageSortBy' "Name"

{-# COMPLETE
  CreationTime,
  Name,
  ModelPackageSortBy' #-}

instance FromText ModelPackageSortBy where
    parser = (ModelPackageSortBy' . mk) <$> takeText

instance ToText ModelPackageSortBy where
    toText (ModelPackageSortBy' ci) = original ci

-- | Represents an enum of /known/ $ModelPackageSortBy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ModelPackageSortBy where
    toEnum i = case i of
        0 -> CreationTime
        1 -> Name
        _ -> (error . showText) $ "Unknown index for ModelPackageSortBy: " <> toText i
    fromEnum x = case x of
        CreationTime -> 0
        Name -> 1
        ModelPackageSortBy' name -> (error . showText) $ "Unknown ModelPackageSortBy: " <> original name

-- | Represents the bounds of /known/ $ModelPackageSortBy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ModelPackageSortBy where
    minBound = CreationTime
    maxBound = Name

instance Hashable     ModelPackageSortBy
instance NFData       ModelPackageSortBy
instance ToByteString ModelPackageSortBy
instance ToQuery      ModelPackageSortBy
instance ToHeader     ModelPackageSortBy

instance ToJSON ModelPackageSortBy where
    toJSON = toJSONText
