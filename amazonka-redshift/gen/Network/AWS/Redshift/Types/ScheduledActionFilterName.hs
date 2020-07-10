{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ScheduledActionFilterName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.ScheduledActionFilterName (
  ScheduledActionFilterName (
    ..
    , ClusterIdentifier
    , IAMRole
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

data ScheduledActionFilterName = ScheduledActionFilterName' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern ClusterIdentifier :: ScheduledActionFilterName
pattern ClusterIdentifier = ScheduledActionFilterName' "cluster-identifier"

pattern IAMRole :: ScheduledActionFilterName
pattern IAMRole = ScheduledActionFilterName' "iam-role"

{-# COMPLETE
  ClusterIdentifier,
  IAMRole,
  ScheduledActionFilterName' #-}

instance FromText ScheduledActionFilterName where
    parser = (ScheduledActionFilterName' . mk) <$> takeText

instance ToText ScheduledActionFilterName where
    toText (ScheduledActionFilterName' ci) = original ci

-- | Represents an enum of /known/ $ScheduledActionFilterName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ScheduledActionFilterName where
    toEnum i = case i of
        0 -> ClusterIdentifier
        1 -> IAMRole
        _ -> (error . showText) $ "Unknown index for ScheduledActionFilterName: " <> toText i
    fromEnum x = case x of
        ClusterIdentifier -> 0
        IAMRole -> 1
        ScheduledActionFilterName' name -> (error . showText) $ "Unknown ScheduledActionFilterName: " <> original name

-- | Represents the bounds of /known/ $ScheduledActionFilterName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ScheduledActionFilterName where
    minBound = ClusterIdentifier
    maxBound = IAMRole

instance Hashable     ScheduledActionFilterName
instance NFData       ScheduledActionFilterName
instance ToByteString ScheduledActionFilterName
instance ToQuery      ScheduledActionFilterName
instance ToHeader     ScheduledActionFilterName
