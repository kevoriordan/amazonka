{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.MergeStrategy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Translate.Types.MergeStrategy (
  MergeStrategy (
    ..
    , Overwrite
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MergeStrategy = MergeStrategy' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern Overwrite :: MergeStrategy
pattern Overwrite = MergeStrategy' "OVERWRITE"

{-# COMPLETE
  Overwrite,
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
        0 -> Overwrite
        _ -> (error . showText) $ "Unknown index for MergeStrategy: " <> toText i
    fromEnum x = case x of
        Overwrite -> 0
        MergeStrategy' name -> (error . showText) $ "Unknown MergeStrategy: " <> original name

-- | Represents the bounds of /known/ $MergeStrategy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MergeStrategy where
    minBound = Overwrite
    maxBound = Overwrite

instance Hashable     MergeStrategy
instance NFData       MergeStrategy
instance ToByteString MergeStrategy
instance ToQuery      MergeStrategy
instance ToHeader     MergeStrategy

instance ToJSON MergeStrategy where
    toJSON = toJSONText
