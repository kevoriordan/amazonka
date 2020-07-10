{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafClientCache
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.CmafClientCache (
  CmafClientCache (
    ..
    , CCCDisabled
    , CCCEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | When set to ENABLED, sets #EXT-X-ALLOW-CACHE:no tag, which prevents client from saving media segments for later replay.
data CmafClientCache = CmafClientCache' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern CCCDisabled :: CmafClientCache
pattern CCCDisabled = CmafClientCache' "DISABLED"

pattern CCCEnabled :: CmafClientCache
pattern CCCEnabled = CmafClientCache' "ENABLED"

{-# COMPLETE
  CCCDisabled,
  CCCEnabled,
  CmafClientCache' #-}

instance FromText CmafClientCache where
    parser = (CmafClientCache' . mk) <$> takeText

instance ToText CmafClientCache where
    toText (CmafClientCache' ci) = original ci

-- | Represents an enum of /known/ $CmafClientCache.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CmafClientCache where
    toEnum i = case i of
        0 -> CCCDisabled
        1 -> CCCEnabled
        _ -> (error . showText) $ "Unknown index for CmafClientCache: " <> toText i
    fromEnum x = case x of
        CCCDisabled -> 0
        CCCEnabled -> 1
        CmafClientCache' name -> (error . showText) $ "Unknown CmafClientCache: " <> original name

-- | Represents the bounds of /known/ $CmafClientCache.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CmafClientCache where
    minBound = CCCDisabled
    maxBound = CCCEnabled

instance Hashable     CmafClientCache
instance NFData       CmafClientCache
instance ToByteString CmafClientCache
instance ToQuery      CmafClientCache
instance ToHeader     CmafClientCache

instance ToJSON CmafClientCache where
    toJSON = toJSONText

instance FromJSON CmafClientCache where
    parseJSON = parseJSONText "CmafClientCache"
