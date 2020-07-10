{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.CacheMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.CacheMode (
  CacheMode (
    ..
    , LocalCustomCache
    , LocalDockerLayerCache
    , LocalSourceCache
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CacheMode = CacheMode' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern LocalCustomCache :: CacheMode
pattern LocalCustomCache = CacheMode' "LOCAL_CUSTOM_CACHE"

pattern LocalDockerLayerCache :: CacheMode
pattern LocalDockerLayerCache = CacheMode' "LOCAL_DOCKER_LAYER_CACHE"

pattern LocalSourceCache :: CacheMode
pattern LocalSourceCache = CacheMode' "LOCAL_SOURCE_CACHE"

{-# COMPLETE
  LocalCustomCache,
  LocalDockerLayerCache,
  LocalSourceCache,
  CacheMode' #-}

instance FromText CacheMode where
    parser = (CacheMode' . mk) <$> takeText

instance ToText CacheMode where
    toText (CacheMode' ci) = original ci

-- | Represents an enum of /known/ $CacheMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CacheMode where
    toEnum i = case i of
        0 -> LocalCustomCache
        1 -> LocalDockerLayerCache
        2 -> LocalSourceCache
        _ -> (error . showText) $ "Unknown index for CacheMode: " <> toText i
    fromEnum x = case x of
        LocalCustomCache -> 0
        LocalDockerLayerCache -> 1
        LocalSourceCache -> 2
        CacheMode' name -> (error . showText) $ "Unknown CacheMode: " <> original name

-- | Represents the bounds of /known/ $CacheMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CacheMode where
    minBound = LocalCustomCache
    maxBound = LocalSourceCache

instance Hashable     CacheMode
instance NFData       CacheMode
instance ToByteString CacheMode
instance ToQuery      CacheMode
instance ToHeader     CacheMode

instance ToJSON CacheMode where
    toJSON = toJSONText

instance FromJSON CacheMode where
    parseJSON = parseJSONText "CacheMode"
