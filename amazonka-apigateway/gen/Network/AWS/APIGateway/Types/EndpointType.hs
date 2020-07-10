{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.EndpointType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.APIGateway.Types.EndpointType (
  EndpointType (
    ..
    , Edge
    , Private
    , Regional
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The endpoint type. The valid values are @EDGE@ for edge-optimized API setup, most suitable for mobile applications; @REGIONAL@ for regional API endpoint setup, most suitable for calling from AWS Region; and @PRIVATE@ for private APIs.
--
--
data EndpointType = EndpointType' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Edge :: EndpointType
pattern Edge = EndpointType' "EDGE"

pattern Private :: EndpointType
pattern Private = EndpointType' "PRIVATE"

pattern Regional :: EndpointType
pattern Regional = EndpointType' "REGIONAL"

{-# COMPLETE
  Edge,
  Private,
  Regional,
  EndpointType' #-}

instance FromText EndpointType where
    parser = (EndpointType' . mk) <$> takeText

instance ToText EndpointType where
    toText (EndpointType' ci) = original ci

-- | Represents an enum of /known/ $EndpointType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EndpointType where
    toEnum i = case i of
        0 -> Edge
        1 -> Private
        2 -> Regional
        _ -> (error . showText) $ "Unknown index for EndpointType: " <> toText i
    fromEnum x = case x of
        Edge -> 0
        Private -> 1
        Regional -> 2
        EndpointType' name -> (error . showText) $ "Unknown EndpointType: " <> original name

-- | Represents the bounds of /known/ $EndpointType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EndpointType where
    minBound = Edge
    maxBound = Regional

instance Hashable     EndpointType
instance NFData       EndpointType
instance ToByteString EndpointType
instance ToQuery      EndpointType
instance ToHeader     EndpointType

instance ToJSON EndpointType where
    toJSON = toJSONText

instance FromJSON EndpointType where
    parseJSON = parseJSONText "EndpointType"
