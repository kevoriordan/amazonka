{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ContinuousExportStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Discovery.Types.ContinuousExportStatus (
  ContinuousExportStatus (
    ..
    , Active
    , Error'
    , Inactive
    , StartFailed
    , StartInProgress
    , StopFailed
    , StopInProgress
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ContinuousExportStatus = ContinuousExportStatus' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern Active :: ContinuousExportStatus
pattern Active = ContinuousExportStatus' "ACTIVE"

pattern Error' :: ContinuousExportStatus
pattern Error' = ContinuousExportStatus' "ERROR"

pattern Inactive :: ContinuousExportStatus
pattern Inactive = ContinuousExportStatus' "INACTIVE"

pattern StartFailed :: ContinuousExportStatus
pattern StartFailed = ContinuousExportStatus' "START_FAILED"

pattern StartInProgress :: ContinuousExportStatus
pattern StartInProgress = ContinuousExportStatus' "START_IN_PROGRESS"

pattern StopFailed :: ContinuousExportStatus
pattern StopFailed = ContinuousExportStatus' "STOP_FAILED"

pattern StopInProgress :: ContinuousExportStatus
pattern StopInProgress = ContinuousExportStatus' "STOP_IN_PROGRESS"

{-# COMPLETE
  Active,
  Error',
  Inactive,
  StartFailed,
  StartInProgress,
  StopFailed,
  StopInProgress,
  ContinuousExportStatus' #-}

instance FromText ContinuousExportStatus where
    parser = (ContinuousExportStatus' . mk) <$> takeText

instance ToText ContinuousExportStatus where
    toText (ContinuousExportStatus' ci) = original ci

-- | Represents an enum of /known/ $ContinuousExportStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ContinuousExportStatus where
    toEnum i = case i of
        0 -> Active
        1 -> Error'
        2 -> Inactive
        3 -> StartFailed
        4 -> StartInProgress
        5 -> StopFailed
        6 -> StopInProgress
        _ -> (error . showText) $ "Unknown index for ContinuousExportStatus: " <> toText i
    fromEnum x = case x of
        Active -> 0
        Error' -> 1
        Inactive -> 2
        StartFailed -> 3
        StartInProgress -> 4
        StopFailed -> 5
        StopInProgress -> 6
        ContinuousExportStatus' name -> (error . showText) $ "Unknown ContinuousExportStatus: " <> original name

-- | Represents the bounds of /known/ $ContinuousExportStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ContinuousExportStatus where
    minBound = Active
    maxBound = StopInProgress

instance Hashable     ContinuousExportStatus
instance NFData       ContinuousExportStatus
instance ToByteString ContinuousExportStatus
instance ToQuery      ContinuousExportStatus
instance ToHeader     ContinuousExportStatus

instance FromJSON ContinuousExportStatus where
    parseJSON = parseJSONText "ContinuousExportStatus"
