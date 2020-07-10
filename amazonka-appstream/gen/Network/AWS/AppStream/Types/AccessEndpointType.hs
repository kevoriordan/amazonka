{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.AccessEndpointType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.AccessEndpointType (
  AccessEndpointType (
    ..
    , Streaming
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AccessEndpointType = AccessEndpointType' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern Streaming :: AccessEndpointType
pattern Streaming = AccessEndpointType' "STREAMING"

{-# COMPLETE
  Streaming,
  AccessEndpointType' #-}

instance FromText AccessEndpointType where
    parser = (AccessEndpointType' . mk) <$> takeText

instance ToText AccessEndpointType where
    toText (AccessEndpointType' ci) = original ci

-- | Represents an enum of /known/ $AccessEndpointType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AccessEndpointType where
    toEnum i = case i of
        0 -> Streaming
        _ -> (error . showText) $ "Unknown index for AccessEndpointType: " <> toText i
    fromEnum x = case x of
        Streaming -> 0
        AccessEndpointType' name -> (error . showText) $ "Unknown AccessEndpointType: " <> original name

-- | Represents the bounds of /known/ $AccessEndpointType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AccessEndpointType where
    minBound = Streaming
    maxBound = Streaming

instance Hashable     AccessEndpointType
instance NFData       AccessEndpointType
instance ToByteString AccessEndpointType
instance ToQuery      AccessEndpointType
instance ToHeader     AccessEndpointType

instance ToJSON AccessEndpointType where
    toJSON = toJSONText

instance FromJSON AccessEndpointType where
    parseJSON = parseJSONText "AccessEndpointType"
