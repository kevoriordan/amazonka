{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AlgorithmStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AlgorithmStatus (
  AlgorithmStatus (
    ..
    , ACompleted
    , ADeleting
    , AFailed
    , AInProgress
    , APending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AlgorithmStatus = AlgorithmStatus' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern ACompleted :: AlgorithmStatus
pattern ACompleted = AlgorithmStatus' "Completed"

pattern ADeleting :: AlgorithmStatus
pattern ADeleting = AlgorithmStatus' "Deleting"

pattern AFailed :: AlgorithmStatus
pattern AFailed = AlgorithmStatus' "Failed"

pattern AInProgress :: AlgorithmStatus
pattern AInProgress = AlgorithmStatus' "InProgress"

pattern APending :: AlgorithmStatus
pattern APending = AlgorithmStatus' "Pending"

{-# COMPLETE
  ACompleted,
  ADeleting,
  AFailed,
  AInProgress,
  APending,
  AlgorithmStatus' #-}

instance FromText AlgorithmStatus where
    parser = (AlgorithmStatus' . mk) <$> takeText

instance ToText AlgorithmStatus where
    toText (AlgorithmStatus' ci) = original ci

-- | Represents an enum of /known/ $AlgorithmStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AlgorithmStatus where
    toEnum i = case i of
        0 -> ACompleted
        1 -> ADeleting
        2 -> AFailed
        3 -> AInProgress
        4 -> APending
        _ -> (error . showText) $ "Unknown index for AlgorithmStatus: " <> toText i
    fromEnum x = case x of
        ACompleted -> 0
        ADeleting -> 1
        AFailed -> 2
        AInProgress -> 3
        APending -> 4
        AlgorithmStatus' name -> (error . showText) $ "Unknown AlgorithmStatus: " <> original name

-- | Represents the bounds of /known/ $AlgorithmStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AlgorithmStatus where
    minBound = ACompleted
    maxBound = APending

instance Hashable     AlgorithmStatus
instance NFData       AlgorithmStatus
instance ToByteString AlgorithmStatus
instance ToQuery      AlgorithmStatus
instance ToHeader     AlgorithmStatus

instance FromJSON AlgorithmStatus where
    parseJSON = parseJSONText "AlgorithmStatus"
