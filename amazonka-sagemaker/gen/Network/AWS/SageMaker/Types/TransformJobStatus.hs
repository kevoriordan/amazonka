{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TransformJobStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.TransformJobStatus (
  TransformJobStatus (
    ..
    , TCompleted
    , TFailed
    , TInProgress
    , TStopped
    , TStopping
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TransformJobStatus = TransformJobStatus' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern TCompleted :: TransformJobStatus
pattern TCompleted = TransformJobStatus' "Completed"

pattern TFailed :: TransformJobStatus
pattern TFailed = TransformJobStatus' "Failed"

pattern TInProgress :: TransformJobStatus
pattern TInProgress = TransformJobStatus' "InProgress"

pattern TStopped :: TransformJobStatus
pattern TStopped = TransformJobStatus' "Stopped"

pattern TStopping :: TransformJobStatus
pattern TStopping = TransformJobStatus' "Stopping"

{-# COMPLETE
  TCompleted,
  TFailed,
  TInProgress,
  TStopped,
  TStopping,
  TransformJobStatus' #-}

instance FromText TransformJobStatus where
    parser = (TransformJobStatus' . mk) <$> takeText

instance ToText TransformJobStatus where
    toText (TransformJobStatus' ci) = original ci

-- | Represents an enum of /known/ $TransformJobStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TransformJobStatus where
    toEnum i = case i of
        0 -> TCompleted
        1 -> TFailed
        2 -> TInProgress
        3 -> TStopped
        4 -> TStopping
        _ -> (error . showText) $ "Unknown index for TransformJobStatus: " <> toText i
    fromEnum x = case x of
        TCompleted -> 0
        TFailed -> 1
        TInProgress -> 2
        TStopped -> 3
        TStopping -> 4
        TransformJobStatus' name -> (error . showText) $ "Unknown TransformJobStatus: " <> original name

-- | Represents the bounds of /known/ $TransformJobStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TransformJobStatus where
    minBound = TCompleted
    maxBound = TStopping

instance Hashable     TransformJobStatus
instance NFData       TransformJobStatus
instance ToByteString TransformJobStatus
instance ToQuery      TransformJobStatus
instance ToHeader     TransformJobStatus

instance ToJSON TransformJobStatus where
    toJSON = toJSONText

instance FromJSON TransformJobStatus where
    parseJSON = parseJSONText "TransformJobStatus"
