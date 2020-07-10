{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.ModelStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.ModelStatus (
  ModelStatus (
    ..
    , MSDeleting
    , MSInError
    , MSStopRequested
    , MSStopped
    , MSSubmitted
    , MSTrained
    , MSTraining
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ModelStatus = ModelStatus' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern MSDeleting :: ModelStatus
pattern MSDeleting = ModelStatus' "DELETING"

pattern MSInError :: ModelStatus
pattern MSInError = ModelStatus' "IN_ERROR"

pattern MSStopRequested :: ModelStatus
pattern MSStopRequested = ModelStatus' "STOP_REQUESTED"

pattern MSStopped :: ModelStatus
pattern MSStopped = ModelStatus' "STOPPED"

pattern MSSubmitted :: ModelStatus
pattern MSSubmitted = ModelStatus' "SUBMITTED"

pattern MSTrained :: ModelStatus
pattern MSTrained = ModelStatus' "TRAINED"

pattern MSTraining :: ModelStatus
pattern MSTraining = ModelStatus' "TRAINING"

{-# COMPLETE
  MSDeleting,
  MSInError,
  MSStopRequested,
  MSStopped,
  MSSubmitted,
  MSTrained,
  MSTraining,
  ModelStatus' #-}

instance FromText ModelStatus where
    parser = (ModelStatus' . mk) <$> takeText

instance ToText ModelStatus where
    toText (ModelStatus' ci) = original ci

-- | Represents an enum of /known/ $ModelStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ModelStatus where
    toEnum i = case i of
        0 -> MSDeleting
        1 -> MSInError
        2 -> MSStopRequested
        3 -> MSStopped
        4 -> MSSubmitted
        5 -> MSTrained
        6 -> MSTraining
        _ -> (error . showText) $ "Unknown index for ModelStatus: " <> toText i
    fromEnum x = case x of
        MSDeleting -> 0
        MSInError -> 1
        MSStopRequested -> 2
        MSStopped -> 3
        MSSubmitted -> 4
        MSTrained -> 5
        MSTraining -> 6
        ModelStatus' name -> (error . showText) $ "Unknown ModelStatus: " <> original name

-- | Represents the bounds of /known/ $ModelStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ModelStatus where
    minBound = MSDeleting
    maxBound = MSTraining

instance Hashable     ModelStatus
instance NFData       ModelStatus
instance ToByteString ModelStatus
instance ToQuery      ModelStatus
instance ToHeader     ModelStatus

instance ToJSON ModelStatus where
    toJSON = toJSONText

instance FromJSON ModelStatus where
    parseJSON = parseJSONText "ModelStatus"
