{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetContentState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.DatasetContentState (
  DatasetContentState (
    ..
    , DCSCreating
    , DCSFailed
    , DCSSucceeded
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DatasetContentState = DatasetContentState' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern DCSCreating :: DatasetContentState
pattern DCSCreating = DatasetContentState' "CREATING"

pattern DCSFailed :: DatasetContentState
pattern DCSFailed = DatasetContentState' "FAILED"

pattern DCSSucceeded :: DatasetContentState
pattern DCSSucceeded = DatasetContentState' "SUCCEEDED"

{-# COMPLETE
  DCSCreating,
  DCSFailed,
  DCSSucceeded,
  DatasetContentState' #-}

instance FromText DatasetContentState where
    parser = (DatasetContentState' . mk) <$> takeText

instance ToText DatasetContentState where
    toText (DatasetContentState' ci) = original ci

-- | Represents an enum of /known/ $DatasetContentState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DatasetContentState where
    toEnum i = case i of
        0 -> DCSCreating
        1 -> DCSFailed
        2 -> DCSSucceeded
        _ -> (error . showText) $ "Unknown index for DatasetContentState: " <> toText i
    fromEnum x = case x of
        DCSCreating -> 0
        DCSFailed -> 1
        DCSSucceeded -> 2
        DatasetContentState' name -> (error . showText) $ "Unknown DatasetContentState: " <> original name

-- | Represents the bounds of /known/ $DatasetContentState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DatasetContentState where
    minBound = DCSCreating
    maxBound = DCSSucceeded

instance Hashable     DatasetContentState
instance NFData       DatasetContentState
instance ToByteString DatasetContentState
instance ToQuery      DatasetContentState
instance ToHeader     DatasetContentState

instance FromJSON DatasetContentState where
    parseJSON = parseJSONText "DatasetContentState"
