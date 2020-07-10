{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.SourceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.SourceType (
  SourceType (
    ..
    , Cluster
    , ClusterParameterGroup
    , ClusterSecurityGroup
    , ClusterSnapshot
    , ScheduledAction
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

data SourceType = SourceType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern Cluster :: SourceType
pattern Cluster = SourceType' "cluster"

pattern ClusterParameterGroup :: SourceType
pattern ClusterParameterGroup = SourceType' "cluster-parameter-group"

pattern ClusterSecurityGroup :: SourceType
pattern ClusterSecurityGroup = SourceType' "cluster-security-group"

pattern ClusterSnapshot :: SourceType
pattern ClusterSnapshot = SourceType' "cluster-snapshot"

pattern ScheduledAction :: SourceType
pattern ScheduledAction = SourceType' "scheduled-action"

{-# COMPLETE
  Cluster,
  ClusterParameterGroup,
  ClusterSecurityGroup,
  ClusterSnapshot,
  ScheduledAction,
  SourceType' #-}

instance FromText SourceType where
    parser = (SourceType' . mk) <$> takeText

instance ToText SourceType where
    toText (SourceType' ci) = original ci

-- | Represents an enum of /known/ $SourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SourceType where
    toEnum i = case i of
        0 -> Cluster
        1 -> ClusterParameterGroup
        2 -> ClusterSecurityGroup
        3 -> ClusterSnapshot
        4 -> ScheduledAction
        _ -> (error . showText) $ "Unknown index for SourceType: " <> toText i
    fromEnum x = case x of
        Cluster -> 0
        ClusterParameterGroup -> 1
        ClusterSecurityGroup -> 2
        ClusterSnapshot -> 3
        ScheduledAction -> 4
        SourceType' name -> (error . showText) $ "Unknown SourceType: " <> original name

-- | Represents the bounds of /known/ $SourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SourceType where
    minBound = Cluster
    maxBound = ScheduledAction

instance Hashable     SourceType
instance NFData       SourceType
instance ToByteString SourceType
instance ToQuery      SourceType
instance ToHeader     SourceType

instance FromXML SourceType where
    parseXML = parseXMLText "SourceType"
