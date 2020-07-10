{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputTimecodeSource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.InputTimecodeSource (
  InputTimecodeSource (
    ..
    , ITSEmbedded
    , ITSZerobased
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Documentation update needed
data InputTimecodeSource = InputTimecodeSource' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern ITSEmbedded :: InputTimecodeSource
pattern ITSEmbedded = InputTimecodeSource' "EMBEDDED"

pattern ITSZerobased :: InputTimecodeSource
pattern ITSZerobased = InputTimecodeSource' "ZEROBASED"

{-# COMPLETE
  ITSEmbedded,
  ITSZerobased,
  InputTimecodeSource' #-}

instance FromText InputTimecodeSource where
    parser = (InputTimecodeSource' . mk) <$> takeText

instance ToText InputTimecodeSource where
    toText (InputTimecodeSource' ci) = original ci

-- | Represents an enum of /known/ $InputTimecodeSource.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InputTimecodeSource where
    toEnum i = case i of
        0 -> ITSEmbedded
        1 -> ITSZerobased
        _ -> (error . showText) $ "Unknown index for InputTimecodeSource: " <> toText i
    fromEnum x = case x of
        ITSEmbedded -> 0
        ITSZerobased -> 1
        InputTimecodeSource' name -> (error . showText) $ "Unknown InputTimecodeSource: " <> original name

-- | Represents the bounds of /known/ $InputTimecodeSource.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InputTimecodeSource where
    minBound = ITSEmbedded
    maxBound = ITSZerobased

instance Hashable     InputTimecodeSource
instance NFData       InputTimecodeSource
instance ToByteString InputTimecodeSource
instance ToQuery      InputTimecodeSource
instance ToHeader     InputTimecodeSource

instance ToJSON InputTimecodeSource where
    toJSON = toJSONText

instance FromJSON InputTimecodeSource where
    parseJSON = parseJSONText "InputTimecodeSource"
