{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.Distribution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchLogs.Types.Distribution (
  Distribution (
    ..
    , ByLogStream
    , Random
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The method used to distribute log data to the destination, which can be either random or grouped by log stream.
--
--
data Distribution = Distribution' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern ByLogStream :: Distribution
pattern ByLogStream = Distribution' "ByLogStream"

pattern Random :: Distribution
pattern Random = Distribution' "Random"

{-# COMPLETE
  ByLogStream,
  Random,
  Distribution' #-}

instance FromText Distribution where
    parser = (Distribution' . mk) <$> takeText

instance ToText Distribution where
    toText (Distribution' ci) = original ci

-- | Represents an enum of /known/ $Distribution.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Distribution where
    toEnum i = case i of
        0 -> ByLogStream
        1 -> Random
        _ -> (error . showText) $ "Unknown index for Distribution: " <> toText i
    fromEnum x = case x of
        ByLogStream -> 0
        Random -> 1
        Distribution' name -> (error . showText) $ "Unknown Distribution: " <> original name

-- | Represents the bounds of /known/ $Distribution.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Distribution where
    minBound = ByLogStream
    maxBound = Random

instance Hashable     Distribution
instance NFData       Distribution
instance ToByteString Distribution
instance ToQuery      Distribution
instance ToHeader     Distribution

instance ToJSON Distribution where
    toJSON = toJSONText

instance FromJSON Distribution where
    parseJSON = parseJSONText "Distribution"
