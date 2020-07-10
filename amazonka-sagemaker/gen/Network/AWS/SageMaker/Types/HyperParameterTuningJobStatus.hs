{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTuningJobStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.HyperParameterTuningJobStatus (
  HyperParameterTuningJobStatus (
    ..
    , HPTJSCompleted
    , HPTJSFailed
    , HPTJSInProgress
    , HPTJSStopped
    , HPTJSStopping
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data HyperParameterTuningJobStatus = HyperParameterTuningJobStatus' (CI
                                                                       Text)
                                       deriving (Eq, Ord, Read, Show, Data,
                                                 Typeable, Generic)

pattern HPTJSCompleted :: HyperParameterTuningJobStatus
pattern HPTJSCompleted = HyperParameterTuningJobStatus' "Completed"

pattern HPTJSFailed :: HyperParameterTuningJobStatus
pattern HPTJSFailed = HyperParameterTuningJobStatus' "Failed"

pattern HPTJSInProgress :: HyperParameterTuningJobStatus
pattern HPTJSInProgress = HyperParameterTuningJobStatus' "InProgress"

pattern HPTJSStopped :: HyperParameterTuningJobStatus
pattern HPTJSStopped = HyperParameterTuningJobStatus' "Stopped"

pattern HPTJSStopping :: HyperParameterTuningJobStatus
pattern HPTJSStopping = HyperParameterTuningJobStatus' "Stopping"

{-# COMPLETE
  HPTJSCompleted,
  HPTJSFailed,
  HPTJSInProgress,
  HPTJSStopped,
  HPTJSStopping,
  HyperParameterTuningJobStatus' #-}

instance FromText HyperParameterTuningJobStatus where
    parser = (HyperParameterTuningJobStatus' . mk) <$> takeText

instance ToText HyperParameterTuningJobStatus where
    toText (HyperParameterTuningJobStatus' ci) = original ci

-- | Represents an enum of /known/ $HyperParameterTuningJobStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HyperParameterTuningJobStatus where
    toEnum i = case i of
        0 -> HPTJSCompleted
        1 -> HPTJSFailed
        2 -> HPTJSInProgress
        3 -> HPTJSStopped
        4 -> HPTJSStopping
        _ -> (error . showText) $ "Unknown index for HyperParameterTuningJobStatus: " <> toText i
    fromEnum x = case x of
        HPTJSCompleted -> 0
        HPTJSFailed -> 1
        HPTJSInProgress -> 2
        HPTJSStopped -> 3
        HPTJSStopping -> 4
        HyperParameterTuningJobStatus' name -> (error . showText) $ "Unknown HyperParameterTuningJobStatus: " <> original name

-- | Represents the bounds of /known/ $HyperParameterTuningJobStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HyperParameterTuningJobStatus where
    minBound = HPTJSCompleted
    maxBound = HPTJSStopping

instance Hashable     HyperParameterTuningJobStatus
instance NFData       HyperParameterTuningJobStatus
instance ToByteString HyperParameterTuningJobStatus
instance ToQuery      HyperParameterTuningJobStatus
instance ToHeader     HyperParameterTuningJobStatus

instance ToJSON HyperParameterTuningJobStatus where
    toJSON = toJSONText

instance FromJSON HyperParameterTuningJobStatus where
    parseJSON = parseJSONText "HyperParameterTuningJobStatus"
