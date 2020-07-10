{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ChannelClass
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.ChannelClass (
  ChannelClass (
    ..
    , CCSinglePipeline
    , CCStandard
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | A standard channel has two encoding pipelines and a single pipeline channel only has one.
data ChannelClass = ChannelClass' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern CCSinglePipeline :: ChannelClass
pattern CCSinglePipeline = ChannelClass' "SINGLE_PIPELINE"

pattern CCStandard :: ChannelClass
pattern CCStandard = ChannelClass' "STANDARD"

{-# COMPLETE
  CCSinglePipeline,
  CCStandard,
  ChannelClass' #-}

instance FromText ChannelClass where
    parser = (ChannelClass' . mk) <$> takeText

instance ToText ChannelClass where
    toText (ChannelClass' ci) = original ci

-- | Represents an enum of /known/ $ChannelClass.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ChannelClass where
    toEnum i = case i of
        0 -> CCSinglePipeline
        1 -> CCStandard
        _ -> (error . showText) $ "Unknown index for ChannelClass: " <> toText i
    fromEnum x = case x of
        CCSinglePipeline -> 0
        CCStandard -> 1
        ChannelClass' name -> (error . showText) $ "Unknown ChannelClass: " <> original name

-- | Represents the bounds of /known/ $ChannelClass.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ChannelClass where
    minBound = CCSinglePipeline
    maxBound = CCStandard

instance Hashable     ChannelClass
instance NFData       ChannelClass
instance ToByteString ChannelClass
instance ToQuery      ChannelClass
instance ToHeader     ChannelClass

instance ToJSON ChannelClass where
    toJSON = toJSONText

instance FromJSON ChannelClass where
    parseJSON = parseJSONText "ChannelClass"
