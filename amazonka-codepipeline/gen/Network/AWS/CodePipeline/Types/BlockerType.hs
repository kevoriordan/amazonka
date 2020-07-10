{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.BlockerType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.BlockerType (
  BlockerType (
    ..
    , Schedule
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data BlockerType = BlockerType' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Schedule :: BlockerType
pattern Schedule = BlockerType' "Schedule"

{-# COMPLETE
  Schedule,
  BlockerType' #-}

instance FromText BlockerType where
    parser = (BlockerType' . mk) <$> takeText

instance ToText BlockerType where
    toText (BlockerType' ci) = original ci

-- | Represents an enum of /known/ $BlockerType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BlockerType where
    toEnum i = case i of
        0 -> Schedule
        _ -> (error . showText) $ "Unknown index for BlockerType: " <> toText i
    fromEnum x = case x of
        Schedule -> 0
        BlockerType' name -> (error . showText) $ "Unknown BlockerType: " <> original name

-- | Represents the bounds of /known/ $BlockerType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BlockerType where
    minBound = Schedule
    maxBound = Schedule

instance Hashable     BlockerType
instance NFData       BlockerType
instance ToByteString BlockerType
instance ToQuery      BlockerType
instance ToHeader     BlockerType

instance ToJSON BlockerType where
    toJSON = toJSONText

instance FromJSON BlockerType where
    parseJSON = parseJSONText "BlockerType"
