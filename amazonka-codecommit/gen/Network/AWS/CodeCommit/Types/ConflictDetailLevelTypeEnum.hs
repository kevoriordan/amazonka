{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ConflictDetailLevelTypeEnum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.ConflictDetailLevelTypeEnum (
  ConflictDetailLevelTypeEnum (
    ..
    , FileLevel
    , LineLevel
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ConflictDetailLevelTypeEnum = ConflictDetailLevelTypeEnum' (CI
                                                                   Text)
                                     deriving (Eq, Ord, Read, Show, Data,
                                               Typeable, Generic)

pattern FileLevel :: ConflictDetailLevelTypeEnum
pattern FileLevel = ConflictDetailLevelTypeEnum' "FILE_LEVEL"

pattern LineLevel :: ConflictDetailLevelTypeEnum
pattern LineLevel = ConflictDetailLevelTypeEnum' "LINE_LEVEL"

{-# COMPLETE
  FileLevel,
  LineLevel,
  ConflictDetailLevelTypeEnum' #-}

instance FromText ConflictDetailLevelTypeEnum where
    parser = (ConflictDetailLevelTypeEnum' . mk) <$> takeText

instance ToText ConflictDetailLevelTypeEnum where
    toText (ConflictDetailLevelTypeEnum' ci) = original ci

-- | Represents an enum of /known/ $ConflictDetailLevelTypeEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ConflictDetailLevelTypeEnum where
    toEnum i = case i of
        0 -> FileLevel
        1 -> LineLevel
        _ -> (error . showText) $ "Unknown index for ConflictDetailLevelTypeEnum: " <> toText i
    fromEnum x = case x of
        FileLevel -> 0
        LineLevel -> 1
        ConflictDetailLevelTypeEnum' name -> (error . showText) $ "Unknown ConflictDetailLevelTypeEnum: " <> original name

-- | Represents the bounds of /known/ $ConflictDetailLevelTypeEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ConflictDetailLevelTypeEnum where
    minBound = FileLevel
    maxBound = LineLevel

instance Hashable     ConflictDetailLevelTypeEnum
instance NFData       ConflictDetailLevelTypeEnum
instance ToByteString ConflictDetailLevelTypeEnum
instance ToQuery      ConflictDetailLevelTypeEnum
instance ToHeader     ConflictDetailLevelTypeEnum

instance ToJSON ConflictDetailLevelTypeEnum where
    toJSON = toJSONText
