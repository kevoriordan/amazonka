{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.MergeOptionTypeEnum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.MergeOptionTypeEnum (
  MergeOptionTypeEnum (
    ..
    , FastForwardMerge
    , SquashMerge
    , ThreeWayMerge
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MergeOptionTypeEnum = MergeOptionTypeEnum' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern FastForwardMerge :: MergeOptionTypeEnum
pattern FastForwardMerge = MergeOptionTypeEnum' "FAST_FORWARD_MERGE"

pattern SquashMerge :: MergeOptionTypeEnum
pattern SquashMerge = MergeOptionTypeEnum' "SQUASH_MERGE"

pattern ThreeWayMerge :: MergeOptionTypeEnum
pattern ThreeWayMerge = MergeOptionTypeEnum' "THREE_WAY_MERGE"

{-# COMPLETE
  FastForwardMerge,
  SquashMerge,
  ThreeWayMerge,
  MergeOptionTypeEnum' #-}

instance FromText MergeOptionTypeEnum where
    parser = (MergeOptionTypeEnum' . mk) <$> takeText

instance ToText MergeOptionTypeEnum where
    toText (MergeOptionTypeEnum' ci) = original ci

-- | Represents an enum of /known/ $MergeOptionTypeEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MergeOptionTypeEnum where
    toEnum i = case i of
        0 -> FastForwardMerge
        1 -> SquashMerge
        2 -> ThreeWayMerge
        _ -> (error . showText) $ "Unknown index for MergeOptionTypeEnum: " <> toText i
    fromEnum x = case x of
        FastForwardMerge -> 0
        SquashMerge -> 1
        ThreeWayMerge -> 2
        MergeOptionTypeEnum' name -> (error . showText) $ "Unknown MergeOptionTypeEnum: " <> original name

-- | Represents the bounds of /known/ $MergeOptionTypeEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MergeOptionTypeEnum where
    minBound = FastForwardMerge
    maxBound = ThreeWayMerge

instance Hashable     MergeOptionTypeEnum
instance NFData       MergeOptionTypeEnum
instance ToByteString MergeOptionTypeEnum
instance ToQuery      MergeOptionTypeEnum
instance ToHeader     MergeOptionTypeEnum

instance ToJSON MergeOptionTypeEnum where
    toJSON = toJSONText

instance FromJSON MergeOptionTypeEnum where
    parseJSON = parseJSONText "MergeOptionTypeEnum"
