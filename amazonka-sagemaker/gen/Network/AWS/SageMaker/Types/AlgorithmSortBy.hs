{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AlgorithmSortBy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AlgorithmSortBy (
  AlgorithmSortBy (
    ..
    , ASBCreationTime
    , ASBName
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AlgorithmSortBy = AlgorithmSortBy' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern ASBCreationTime :: AlgorithmSortBy
pattern ASBCreationTime = AlgorithmSortBy' "CreationTime"

pattern ASBName :: AlgorithmSortBy
pattern ASBName = AlgorithmSortBy' "Name"

{-# COMPLETE
  ASBCreationTime,
  ASBName,
  AlgorithmSortBy' #-}

instance FromText AlgorithmSortBy where
    parser = (AlgorithmSortBy' . mk) <$> takeText

instance ToText AlgorithmSortBy where
    toText (AlgorithmSortBy' ci) = original ci

-- | Represents an enum of /known/ $AlgorithmSortBy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AlgorithmSortBy where
    toEnum i = case i of
        0 -> ASBCreationTime
        1 -> ASBName
        _ -> (error . showText) $ "Unknown index for AlgorithmSortBy: " <> toText i
    fromEnum x = case x of
        ASBCreationTime -> 0
        ASBName -> 1
        AlgorithmSortBy' name -> (error . showText) $ "Unknown AlgorithmSortBy: " <> original name

-- | Represents the bounds of /known/ $AlgorithmSortBy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AlgorithmSortBy where
    minBound = ASBCreationTime
    maxBound = ASBName

instance Hashable     AlgorithmSortBy
instance NFData       AlgorithmSortBy
instance ToByteString AlgorithmSortBy
instance ToQuery      AlgorithmSortBy
instance ToHeader     AlgorithmSortBy

instance ToJSON AlgorithmSortBy where
    toJSON = toJSONText
