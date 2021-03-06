{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ScheduledActionTypeValues
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.ScheduledActionTypeValues (
  ScheduledActionTypeValues (
    ..
    , PauseCluster
    , ResizeCluster
    , ResumeCluster
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

data ScheduledActionTypeValues = ScheduledActionTypeValues' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern PauseCluster :: ScheduledActionTypeValues
pattern PauseCluster = ScheduledActionTypeValues' "PauseCluster"

pattern ResizeCluster :: ScheduledActionTypeValues
pattern ResizeCluster = ScheduledActionTypeValues' "ResizeCluster"

pattern ResumeCluster :: ScheduledActionTypeValues
pattern ResumeCluster = ScheduledActionTypeValues' "ResumeCluster"

{-# COMPLETE
  PauseCluster,
  ResizeCluster,
  ResumeCluster,
  ScheduledActionTypeValues' #-}

instance FromText ScheduledActionTypeValues where
    parser = (ScheduledActionTypeValues' . mk) <$> takeText

instance ToText ScheduledActionTypeValues where
    toText (ScheduledActionTypeValues' ci) = original ci

-- | Represents an enum of /known/ $ScheduledActionTypeValues.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ScheduledActionTypeValues where
    toEnum i = case i of
        0 -> PauseCluster
        1 -> ResizeCluster
        2 -> ResumeCluster
        _ -> (error . showText) $ "Unknown index for ScheduledActionTypeValues: " <> toText i
    fromEnum x = case x of
        PauseCluster -> 0
        ResizeCluster -> 1
        ResumeCluster -> 2
        ScheduledActionTypeValues' name -> (error . showText) $ "Unknown ScheduledActionTypeValues: " <> original name

-- | Represents the bounds of /known/ $ScheduledActionTypeValues.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ScheduledActionTypeValues where
    minBound = PauseCluster
    maxBound = ResumeCluster

instance Hashable     ScheduledActionTypeValues
instance NFData       ScheduledActionTypeValues
instance ToByteString ScheduledActionTypeValues
instance ToQuery      ScheduledActionTypeValues
instance ToHeader     ScheduledActionTypeValues
