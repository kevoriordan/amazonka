{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.DatasetStatus (
  DatasetStatus (
    ..
    , Active
    , Creating
    , Deleting
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DatasetStatus = DatasetStatus' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern Active :: DatasetStatus
pattern Active = DatasetStatus' "ACTIVE"

pattern Creating :: DatasetStatus
pattern Creating = DatasetStatus' "CREATING"

pattern Deleting :: DatasetStatus
pattern Deleting = DatasetStatus' "DELETING"

{-# COMPLETE
  Active,
  Creating,
  Deleting,
  DatasetStatus' #-}

instance FromText DatasetStatus where
    parser = (DatasetStatus' . mk) <$> takeText

instance ToText DatasetStatus where
    toText (DatasetStatus' ci) = original ci

-- | Represents an enum of /known/ $DatasetStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DatasetStatus where
    toEnum i = case i of
        0 -> Active
        1 -> Creating
        2 -> Deleting
        _ -> (error . showText) $ "Unknown index for DatasetStatus: " <> toText i
    fromEnum x = case x of
        Active -> 0
        Creating -> 1
        Deleting -> 2
        DatasetStatus' name -> (error . showText) $ "Unknown DatasetStatus: " <> original name

-- | Represents the bounds of /known/ $DatasetStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DatasetStatus where
    minBound = Active
    maxBound = Deleting

instance Hashable     DatasetStatus
instance NFData       DatasetStatus
instance ToByteString DatasetStatus
instance ToQuery      DatasetStatus
instance ToHeader     DatasetStatus

instance FromJSON DatasetStatus where
    parseJSON = parseJSONText "DatasetStatus"
