{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelingJobStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.LabelingJobStatus (
  LabelingJobStatus (
    ..
    , LJSCompleted
    , LJSFailed
    , LJSInProgress
    , LJSStopped
    , LJSStopping
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LabelingJobStatus = LabelingJobStatus' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern LJSCompleted :: LabelingJobStatus
pattern LJSCompleted = LabelingJobStatus' "Completed"

pattern LJSFailed :: LabelingJobStatus
pattern LJSFailed = LabelingJobStatus' "Failed"

pattern LJSInProgress :: LabelingJobStatus
pattern LJSInProgress = LabelingJobStatus' "InProgress"

pattern LJSStopped :: LabelingJobStatus
pattern LJSStopped = LabelingJobStatus' "Stopped"

pattern LJSStopping :: LabelingJobStatus
pattern LJSStopping = LabelingJobStatus' "Stopping"

{-# COMPLETE
  LJSCompleted,
  LJSFailed,
  LJSInProgress,
  LJSStopped,
  LJSStopping,
  LabelingJobStatus' #-}

instance FromText LabelingJobStatus where
    parser = (LabelingJobStatus' . mk) <$> takeText

instance ToText LabelingJobStatus where
    toText (LabelingJobStatus' ci) = original ci

-- | Represents an enum of /known/ $LabelingJobStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LabelingJobStatus where
    toEnum i = case i of
        0 -> LJSCompleted
        1 -> LJSFailed
        2 -> LJSInProgress
        3 -> LJSStopped
        4 -> LJSStopping
        _ -> (error . showText) $ "Unknown index for LabelingJobStatus: " <> toText i
    fromEnum x = case x of
        LJSCompleted -> 0
        LJSFailed -> 1
        LJSInProgress -> 2
        LJSStopped -> 3
        LJSStopping -> 4
        LabelingJobStatus' name -> (error . showText) $ "Unknown LabelingJobStatus: " <> original name

-- | Represents the bounds of /known/ $LabelingJobStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LabelingJobStatus where
    minBound = LJSCompleted
    maxBound = LJSStopping

instance Hashable     LabelingJobStatus
instance NFData       LabelingJobStatus
instance ToByteString LabelingJobStatus
instance ToQuery      LabelingJobStatus
instance ToHeader     LabelingJobStatus

instance ToJSON LabelingJobStatus where
    toJSON = toJSONText

instance FromJSON LabelingJobStatus where
    parseJSON = parseJSONText "LabelingJobStatus"
