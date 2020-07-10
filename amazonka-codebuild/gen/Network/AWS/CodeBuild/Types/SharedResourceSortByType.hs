{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.SharedResourceSortByType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.SharedResourceSortByType (
  SharedResourceSortByType (
    ..
    , ARN
    , ModifiedTime
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SharedResourceSortByType = SharedResourceSortByType' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern ARN :: SharedResourceSortByType
pattern ARN = SharedResourceSortByType' "ARN"

pattern ModifiedTime :: SharedResourceSortByType
pattern ModifiedTime = SharedResourceSortByType' "MODIFIED_TIME"

{-# COMPLETE
  ARN,
  ModifiedTime,
  SharedResourceSortByType' #-}

instance FromText SharedResourceSortByType where
    parser = (SharedResourceSortByType' . mk) <$> takeText

instance ToText SharedResourceSortByType where
    toText (SharedResourceSortByType' ci) = original ci

-- | Represents an enum of /known/ $SharedResourceSortByType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SharedResourceSortByType where
    toEnum i = case i of
        0 -> ARN
        1 -> ModifiedTime
        _ -> (error . showText) $ "Unknown index for SharedResourceSortByType: " <> toText i
    fromEnum x = case x of
        ARN -> 0
        ModifiedTime -> 1
        SharedResourceSortByType' name -> (error . showText) $ "Unknown SharedResourceSortByType: " <> original name

-- | Represents the bounds of /known/ $SharedResourceSortByType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SharedResourceSortByType where
    minBound = ARN
    maxBound = ModifiedTime

instance Hashable     SharedResourceSortByType
instance NFData       SharedResourceSortByType
instance ToByteString SharedResourceSortByType
instance ToQuery      SharedResourceSortByType
instance ToHeader     SharedResourceSortByType

instance ToJSON SharedResourceSortByType where
    toJSON = toJSONText
