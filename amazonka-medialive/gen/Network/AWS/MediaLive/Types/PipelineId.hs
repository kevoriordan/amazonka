{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.PipelineId
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.PipelineId (
  PipelineId (
    ..
    , PIPipeline0
    , PIPipeline1
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Pipeline ID
data PipelineId = PipelineId' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern PIPipeline0 :: PipelineId
pattern PIPipeline0 = PipelineId' "PIPELINE_0"

pattern PIPipeline1 :: PipelineId
pattern PIPipeline1 = PipelineId' "PIPELINE_1"

{-# COMPLETE
  PIPipeline0,
  PIPipeline1,
  PipelineId' #-}

instance FromText PipelineId where
    parser = (PipelineId' . mk) <$> takeText

instance ToText PipelineId where
    toText (PipelineId' ci) = original ci

-- | Represents an enum of /known/ $PipelineId.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PipelineId where
    toEnum i = case i of
        0 -> PIPipeline0
        1 -> PIPipeline1
        _ -> (error . showText) $ "Unknown index for PipelineId: " <> toText i
    fromEnum x = case x of
        PIPipeline0 -> 0
        PIPipeline1 -> 1
        PipelineId' name -> (error . showText) $ "Unknown PipelineId: " <> original name

-- | Represents the bounds of /known/ $PipelineId.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PipelineId where
    minBound = PIPipeline0
    maxBound = PIPipeline1

instance Hashable     PipelineId
instance NFData       PipelineId
instance ToByteString PipelineId
instance ToQuery      PipelineId
instance ToHeader     PipelineId

instance ToJSON PipelineId where
    toJSON = toJSONText

instance FromJSON PipelineId where
    parseJSON = parseJSONText "PipelineId"
