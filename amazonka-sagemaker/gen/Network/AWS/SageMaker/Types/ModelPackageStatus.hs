{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ModelPackageStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ModelPackageStatus (
  ModelPackageStatus (
    ..
    , MPSCompleted
    , MPSDeleting
    , MPSFailed
    , MPSInProgress
    , MPSPending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ModelPackageStatus = ModelPackageStatus' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern MPSCompleted :: ModelPackageStatus
pattern MPSCompleted = ModelPackageStatus' "Completed"

pattern MPSDeleting :: ModelPackageStatus
pattern MPSDeleting = ModelPackageStatus' "Deleting"

pattern MPSFailed :: ModelPackageStatus
pattern MPSFailed = ModelPackageStatus' "Failed"

pattern MPSInProgress :: ModelPackageStatus
pattern MPSInProgress = ModelPackageStatus' "InProgress"

pattern MPSPending :: ModelPackageStatus
pattern MPSPending = ModelPackageStatus' "Pending"

{-# COMPLETE
  MPSCompleted,
  MPSDeleting,
  MPSFailed,
  MPSInProgress,
  MPSPending,
  ModelPackageStatus' #-}

instance FromText ModelPackageStatus where
    parser = (ModelPackageStatus' . mk) <$> takeText

instance ToText ModelPackageStatus where
    toText (ModelPackageStatus' ci) = original ci

-- | Represents an enum of /known/ $ModelPackageStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ModelPackageStatus where
    toEnum i = case i of
        0 -> MPSCompleted
        1 -> MPSDeleting
        2 -> MPSFailed
        3 -> MPSInProgress
        4 -> MPSPending
        _ -> (error . showText) $ "Unknown index for ModelPackageStatus: " <> toText i
    fromEnum x = case x of
        MPSCompleted -> 0
        MPSDeleting -> 1
        MPSFailed -> 2
        MPSInProgress -> 3
        MPSPending -> 4
        ModelPackageStatus' name -> (error . showText) $ "Unknown ModelPackageStatus: " <> original name

-- | Represents the bounds of /known/ $ModelPackageStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ModelPackageStatus where
    minBound = MPSCompleted
    maxBound = MPSPending

instance Hashable     ModelPackageStatus
instance NFData       ModelPackageStatus
instance ToByteString ModelPackageStatus
instance ToQuery      ModelPackageStatus
instance ToHeader     ModelPackageStatus

instance FromJSON ModelPackageStatus where
    parseJSON = parseJSONText "ModelPackageStatus"
