{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.MergeStrategy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.MergeStrategy (
  MergeStrategy (
    ..
    , FailOnConflict
    , OverwriteLatest
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MergeStrategy = MergeStrategy' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern FailOnConflict :: MergeStrategy
pattern FailOnConflict = MergeStrategy' "FAIL_ON_CONFLICT"

pattern OverwriteLatest :: MergeStrategy
pattern OverwriteLatest = MergeStrategy' "OVERWRITE_LATEST"

{-# COMPLETE
  FailOnConflict,
  OverwriteLatest,
  MergeStrategy' #-}

instance FromText MergeStrategy where
    parser = (MergeStrategy' . mk) <$> takeText

instance ToText MergeStrategy where
    toText (MergeStrategy' ci) = original ci

-- | Represents an enum of /known/ $MergeStrategy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MergeStrategy where
    toEnum i = case i of
        0 -> FailOnConflict
        1 -> OverwriteLatest
        _ -> (error . showText) $ "Unknown index for MergeStrategy: " <> toText i
    fromEnum x = case x of
        FailOnConflict -> 0
        OverwriteLatest -> 1
        MergeStrategy' name -> (error . showText) $ "Unknown MergeStrategy: " <> original name

-- | Represents the bounds of /known/ $MergeStrategy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MergeStrategy where
    minBound = FailOnConflict
    maxBound = OverwriteLatest

instance Hashable     MergeStrategy
instance NFData       MergeStrategy
instance ToByteString MergeStrategy
instance ToQuery      MergeStrategy
instance ToHeader     MergeStrategy

instance ToJSON MergeStrategy where
    toJSON = toJSONText

instance FromJSON MergeStrategy where
    parseJSON = parseJSONText "MergeStrategy"
