{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ReplacementTypeEnum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.ReplacementTypeEnum (
  ReplacementTypeEnum (
    ..
    , KeepBase
    , KeepDestination
    , KeepSource
    , UseNewContent
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ReplacementTypeEnum = ReplacementTypeEnum' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern KeepBase :: ReplacementTypeEnum
pattern KeepBase = ReplacementTypeEnum' "KEEP_BASE"

pattern KeepDestination :: ReplacementTypeEnum
pattern KeepDestination = ReplacementTypeEnum' "KEEP_DESTINATION"

pattern KeepSource :: ReplacementTypeEnum
pattern KeepSource = ReplacementTypeEnum' "KEEP_SOURCE"

pattern UseNewContent :: ReplacementTypeEnum
pattern UseNewContent = ReplacementTypeEnum' "USE_NEW_CONTENT"

{-# COMPLETE
  KeepBase,
  KeepDestination,
  KeepSource,
  UseNewContent,
  ReplacementTypeEnum' #-}

instance FromText ReplacementTypeEnum where
    parser = (ReplacementTypeEnum' . mk) <$> takeText

instance ToText ReplacementTypeEnum where
    toText (ReplacementTypeEnum' ci) = original ci

-- | Represents an enum of /known/ $ReplacementTypeEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReplacementTypeEnum where
    toEnum i = case i of
        0 -> KeepBase
        1 -> KeepDestination
        2 -> KeepSource
        3 -> UseNewContent
        _ -> (error . showText) $ "Unknown index for ReplacementTypeEnum: " <> toText i
    fromEnum x = case x of
        KeepBase -> 0
        KeepDestination -> 1
        KeepSource -> 2
        UseNewContent -> 3
        ReplacementTypeEnum' name -> (error . showText) $ "Unknown ReplacementTypeEnum: " <> original name

-- | Represents the bounds of /known/ $ReplacementTypeEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReplacementTypeEnum where
    minBound = KeepBase
    maxBound = UseNewContent

instance Hashable     ReplacementTypeEnum
instance NFData       ReplacementTypeEnum
instance ToByteString ReplacementTypeEnum
instance ToQuery      ReplacementTypeEnum
instance ToHeader     ReplacementTypeEnum

instance ToJSON ReplacementTypeEnum where
    toJSON = toJSONText
