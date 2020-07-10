{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.ConsistencyLevel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.ConsistencyLevel (
  ConsistencyLevel (
    ..
    , Eventual
    , Serializable
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ConsistencyLevel = ConsistencyLevel' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern Eventual :: ConsistencyLevel
pattern Eventual = ConsistencyLevel' "EVENTUAL"

pattern Serializable :: ConsistencyLevel
pattern Serializable = ConsistencyLevel' "SERIALIZABLE"

{-# COMPLETE
  Eventual,
  Serializable,
  ConsistencyLevel' #-}

instance FromText ConsistencyLevel where
    parser = (ConsistencyLevel' . mk) <$> takeText

instance ToText ConsistencyLevel where
    toText (ConsistencyLevel' ci) = original ci

-- | Represents an enum of /known/ $ConsistencyLevel.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ConsistencyLevel where
    toEnum i = case i of
        0 -> Eventual
        1 -> Serializable
        _ -> (error . showText) $ "Unknown index for ConsistencyLevel: " <> toText i
    fromEnum x = case x of
        Eventual -> 0
        Serializable -> 1
        ConsistencyLevel' name -> (error . showText) $ "Unknown ConsistencyLevel: " <> original name

-- | Represents the bounds of /known/ $ConsistencyLevel.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ConsistencyLevel where
    minBound = Eventual
    maxBound = Serializable

instance Hashable     ConsistencyLevel
instance NFData       ConsistencyLevel
instance ToByteString ConsistencyLevel
instance ToQuery      ConsistencyLevel
instance ToHeader     ConsistencyLevel

instance ToJSON ConsistencyLevel where
    toJSON = toJSONText
