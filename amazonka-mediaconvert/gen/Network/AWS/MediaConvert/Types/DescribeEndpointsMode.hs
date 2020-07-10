{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DescribeEndpointsMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.DescribeEndpointsMode (
  DescribeEndpointsMode (
    ..
    , DEMDefault
    , DEMGetOnly
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Optional field, defaults to DEFAULT. Specify DEFAULT for this operation to return your endpoints if any exist, or to create an endpoint for you and return it if one doesn't already exist. Specify GET_ONLY to return your endpoints if any exist, or an empty list if none exist.
data DescribeEndpointsMode = DescribeEndpointsMode' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern DEMDefault :: DescribeEndpointsMode
pattern DEMDefault = DescribeEndpointsMode' "DEFAULT"

pattern DEMGetOnly :: DescribeEndpointsMode
pattern DEMGetOnly = DescribeEndpointsMode' "GET_ONLY"

{-# COMPLETE
  DEMDefault,
  DEMGetOnly,
  DescribeEndpointsMode' #-}

instance FromText DescribeEndpointsMode where
    parser = (DescribeEndpointsMode' . mk) <$> takeText

instance ToText DescribeEndpointsMode where
    toText (DescribeEndpointsMode' ci) = original ci

-- | Represents an enum of /known/ $DescribeEndpointsMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DescribeEndpointsMode where
    toEnum i = case i of
        0 -> DEMDefault
        1 -> DEMGetOnly
        _ -> (error . showText) $ "Unknown index for DescribeEndpointsMode: " <> toText i
    fromEnum x = case x of
        DEMDefault -> 0
        DEMGetOnly -> 1
        DescribeEndpointsMode' name -> (error . showText) $ "Unknown DescribeEndpointsMode: " <> original name

-- | Represents the bounds of /known/ $DescribeEndpointsMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DescribeEndpointsMode where
    minBound = DEMDefault
    maxBound = DEMGetOnly

instance Hashable     DescribeEndpointsMode
instance NFData       DescribeEndpointsMode
instance ToByteString DescribeEndpointsMode
instance ToQuery      DescribeEndpointsMode
instance ToHeader     DescribeEndpointsMode

instance ToJSON DescribeEndpointsMode where
    toJSON = toJSONText
