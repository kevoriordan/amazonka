{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.TimedMetadata
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.TimedMetadata (
  TimedMetadata (
    ..
    , TMNone
    , TMPassthrough
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Applies only to HLS outputs. Use this setting to specify whether the service inserts the ID3 timed metadata from the input in this output.
data TimedMetadata = TimedMetadata' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern TMNone :: TimedMetadata
pattern TMNone = TimedMetadata' "NONE"

pattern TMPassthrough :: TimedMetadata
pattern TMPassthrough = TimedMetadata' "PASSTHROUGH"

{-# COMPLETE
  TMNone,
  TMPassthrough,
  TimedMetadata' #-}

instance FromText TimedMetadata where
    parser = (TimedMetadata' . mk) <$> takeText

instance ToText TimedMetadata where
    toText (TimedMetadata' ci) = original ci

-- | Represents an enum of /known/ $TimedMetadata.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TimedMetadata where
    toEnum i = case i of
        0 -> TMNone
        1 -> TMPassthrough
        _ -> (error . showText) $ "Unknown index for TimedMetadata: " <> toText i
    fromEnum x = case x of
        TMNone -> 0
        TMPassthrough -> 1
        TimedMetadata' name -> (error . showText) $ "Unknown TimedMetadata: " <> original name

-- | Represents the bounds of /known/ $TimedMetadata.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TimedMetadata where
    minBound = TMNone
    maxBound = TMPassthrough

instance Hashable     TimedMetadata
instance NFData       TimedMetadata
instance ToByteString TimedMetadata
instance ToQuery      TimedMetadata
instance ToHeader     TimedMetadata

instance ToJSON TimedMetadata where
    toJSON = toJSONText

instance FromJSON TimedMetadata where
    parseJSON = parseJSONText "TimedMetadata"
