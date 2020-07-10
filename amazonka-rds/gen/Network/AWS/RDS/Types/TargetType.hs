{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.TargetType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.TargetType (
  TargetType (
    ..
    , RDSInstance
    , RDSServerlessEndpoint
    , TrackedCluster
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TargetType = TargetType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern RDSInstance :: TargetType
pattern RDSInstance = TargetType' "RDS_INSTANCE"

pattern RDSServerlessEndpoint :: TargetType
pattern RDSServerlessEndpoint = TargetType' "RDS_SERVERLESS_ENDPOINT"

pattern TrackedCluster :: TargetType
pattern TrackedCluster = TargetType' "TRACKED_CLUSTER"

{-# COMPLETE
  RDSInstance,
  RDSServerlessEndpoint,
  TrackedCluster,
  TargetType' #-}

instance FromText TargetType where
    parser = (TargetType' . mk) <$> takeText

instance ToText TargetType where
    toText (TargetType' ci) = original ci

-- | Represents an enum of /known/ $TargetType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TargetType where
    toEnum i = case i of
        0 -> RDSInstance
        1 -> RDSServerlessEndpoint
        2 -> TrackedCluster
        _ -> (error . showText) $ "Unknown index for TargetType: " <> toText i
    fromEnum x = case x of
        RDSInstance -> 0
        RDSServerlessEndpoint -> 1
        TrackedCluster -> 2
        TargetType' name -> (error . showText) $ "Unknown TargetType: " <> original name

-- | Represents the bounds of /known/ $TargetType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TargetType where
    minBound = RDSInstance
    maxBound = TrackedCluster

instance Hashable     TargetType
instance NFData       TargetType
instance ToByteString TargetType
instance ToQuery      TargetType
instance ToHeader     TargetType

instance FromXML TargetType where
    parseXML = parseXMLText "TargetType"
