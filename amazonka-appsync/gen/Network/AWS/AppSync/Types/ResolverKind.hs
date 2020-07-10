{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.ResolverKind
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.ResolverKind (
  ResolverKind (
    ..
    , Pipeline
    , Unit
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ResolverKind = ResolverKind' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Pipeline :: ResolverKind
pattern Pipeline = ResolverKind' "PIPELINE"

pattern Unit :: ResolverKind
pattern Unit = ResolverKind' "UNIT"

{-# COMPLETE
  Pipeline,
  Unit,
  ResolverKind' #-}

instance FromText ResolverKind where
    parser = (ResolverKind' . mk) <$> takeText

instance ToText ResolverKind where
    toText (ResolverKind' ci) = original ci

-- | Represents an enum of /known/ $ResolverKind.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ResolverKind where
    toEnum i = case i of
        0 -> Pipeline
        1 -> Unit
        _ -> (error . showText) $ "Unknown index for ResolverKind: " <> toText i
    fromEnum x = case x of
        Pipeline -> 0
        Unit -> 1
        ResolverKind' name -> (error . showText) $ "Unknown ResolverKind: " <> original name

-- | Represents the bounds of /known/ $ResolverKind.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ResolverKind where
    minBound = Pipeline
    maxBound = Unit

instance Hashable     ResolverKind
instance NFData       ResolverKind
instance ToByteString ResolverKind
instance ToQuery      ResolverKind
instance ToHeader     ResolverKind

instance ToJSON ResolverKind where
    toJSON = toJSONText

instance FromJSON ResolverKind where
    parseJSON = parseJSONText "ResolverKind"
