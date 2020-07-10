{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.NetworkProfileType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.NetworkProfileType (
  NetworkProfileType (
    ..
    , Curated
    , Private
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data NetworkProfileType = NetworkProfileType' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern Curated :: NetworkProfileType
pattern Curated = NetworkProfileType' "CURATED"

pattern Private :: NetworkProfileType
pattern Private = NetworkProfileType' "PRIVATE"

{-# COMPLETE
  Curated,
  Private,
  NetworkProfileType' #-}

instance FromText NetworkProfileType where
    parser = (NetworkProfileType' . mk) <$> takeText

instance ToText NetworkProfileType where
    toText (NetworkProfileType' ci) = original ci

-- | Represents an enum of /known/ $NetworkProfileType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum NetworkProfileType where
    toEnum i = case i of
        0 -> Curated
        1 -> Private
        _ -> (error . showText) $ "Unknown index for NetworkProfileType: " <> toText i
    fromEnum x = case x of
        Curated -> 0
        Private -> 1
        NetworkProfileType' name -> (error . showText) $ "Unknown NetworkProfileType: " <> original name

-- | Represents the bounds of /known/ $NetworkProfileType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded NetworkProfileType where
    minBound = Curated
    maxBound = Private

instance Hashable     NetworkProfileType
instance NFData       NetworkProfileType
instance ToByteString NetworkProfileType
instance ToQuery      NetworkProfileType
instance ToHeader     NetworkProfileType

instance ToJSON NetworkProfileType where
    toJSON = toJSONText

instance FromJSON NetworkProfileType where
    parseJSON = parseJSONText "NetworkProfileType"
