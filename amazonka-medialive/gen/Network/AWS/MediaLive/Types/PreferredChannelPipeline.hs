{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.PreferredChannelPipeline
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.PreferredChannelPipeline (
  PreferredChannelPipeline (
    ..
    , CurrentlyActive
    , Pipeline0
    , Pipeline1
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Indicates which pipeline is preferred by the multiplex for program ingest.
--
-- If set to \"PIPELINE_0\" or \"PIPELINE_1\" and an unhealthy ingest causes the multiplex to switch to the non-preferred pipeline,
-- it will switch back once that ingest is healthy again. If set to \"CURRENTLY_ACTIVE\",
-- it will not switch back to the other pipeline based on it recovering to a healthy state,
-- it will only switch if the active pipeline becomes unhealthy.
data PreferredChannelPipeline = PreferredChannelPipeline' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern CurrentlyActive :: PreferredChannelPipeline
pattern CurrentlyActive = PreferredChannelPipeline' "CURRENTLY_ACTIVE"

pattern Pipeline0 :: PreferredChannelPipeline
pattern Pipeline0 = PreferredChannelPipeline' "PIPELINE_0"

pattern Pipeline1 :: PreferredChannelPipeline
pattern Pipeline1 = PreferredChannelPipeline' "PIPELINE_1"

{-# COMPLETE
  CurrentlyActive,
  Pipeline0,
  Pipeline1,
  PreferredChannelPipeline' #-}

instance FromText PreferredChannelPipeline where
    parser = (PreferredChannelPipeline' . mk) <$> takeText

instance ToText PreferredChannelPipeline where
    toText (PreferredChannelPipeline' ci) = original ci

-- | Represents an enum of /known/ $PreferredChannelPipeline.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PreferredChannelPipeline where
    toEnum i = case i of
        0 -> CurrentlyActive
        1 -> Pipeline0
        2 -> Pipeline1
        _ -> (error . showText) $ "Unknown index for PreferredChannelPipeline: " <> toText i
    fromEnum x = case x of
        CurrentlyActive -> 0
        Pipeline0 -> 1
        Pipeline1 -> 2
        PreferredChannelPipeline' name -> (error . showText) $ "Unknown PreferredChannelPipeline: " <> original name

-- | Represents the bounds of /known/ $PreferredChannelPipeline.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PreferredChannelPipeline where
    minBound = CurrentlyActive
    maxBound = Pipeline1

instance Hashable     PreferredChannelPipeline
instance NFData       PreferredChannelPipeline
instance ToByteString PreferredChannelPipeline
instance ToQuery      PreferredChannelPipeline
instance ToHeader     PreferredChannelPipeline

instance ToJSON PreferredChannelPipeline where
    toJSON = toJSONText

instance FromJSON PreferredChannelPipeline where
    parseJSON = parseJSONText "PreferredChannelPipeline"
