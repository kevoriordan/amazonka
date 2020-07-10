{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.DetailedModelPackageStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.DetailedModelPackageStatus (
  DetailedModelPackageStatus (
    ..
    , DMPSCompleted
    , DMPSFailed
    , DMPSInProgress
    , DMPSNotStarted
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DetailedModelPackageStatus = DetailedModelPackageStatus' (CI
                                                                 Text)
                                    deriving (Eq, Ord, Read, Show, Data,
                                              Typeable, Generic)

pattern DMPSCompleted :: DetailedModelPackageStatus
pattern DMPSCompleted = DetailedModelPackageStatus' "Completed"

pattern DMPSFailed :: DetailedModelPackageStatus
pattern DMPSFailed = DetailedModelPackageStatus' "Failed"

pattern DMPSInProgress :: DetailedModelPackageStatus
pattern DMPSInProgress = DetailedModelPackageStatus' "InProgress"

pattern DMPSNotStarted :: DetailedModelPackageStatus
pattern DMPSNotStarted = DetailedModelPackageStatus' "NotStarted"

{-# COMPLETE
  DMPSCompleted,
  DMPSFailed,
  DMPSInProgress,
  DMPSNotStarted,
  DetailedModelPackageStatus' #-}

instance FromText DetailedModelPackageStatus where
    parser = (DetailedModelPackageStatus' . mk) <$> takeText

instance ToText DetailedModelPackageStatus where
    toText (DetailedModelPackageStatus' ci) = original ci

-- | Represents an enum of /known/ $DetailedModelPackageStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DetailedModelPackageStatus where
    toEnum i = case i of
        0 -> DMPSCompleted
        1 -> DMPSFailed
        2 -> DMPSInProgress
        3 -> DMPSNotStarted
        _ -> (error . showText) $ "Unknown index for DetailedModelPackageStatus: " <> toText i
    fromEnum x = case x of
        DMPSCompleted -> 0
        DMPSFailed -> 1
        DMPSInProgress -> 2
        DMPSNotStarted -> 3
        DetailedModelPackageStatus' name -> (error . showText) $ "Unknown DetailedModelPackageStatus: " <> original name

-- | Represents the bounds of /known/ $DetailedModelPackageStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DetailedModelPackageStatus where
    minBound = DMPSCompleted
    maxBound = DMPSNotStarted

instance Hashable     DetailedModelPackageStatus
instance NFData       DetailedModelPackageStatus
instance ToByteString DetailedModelPackageStatus
instance ToQuery      DetailedModelPackageStatus
instance ToHeader     DetailedModelPackageStatus

instance FromJSON DetailedModelPackageStatus where
    parseJSON = parseJSONText "DetailedModelPackageStatus"
