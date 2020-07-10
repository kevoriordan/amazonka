{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLJobSecondaryStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AutoMLJobSecondaryStatus (
  AutoMLJobSecondaryStatus (
    ..
    , AMLJSSAnalyzingData
    , AMLJSSCandidateDefinitionsGenerated
    , AMLJSSFailed
    , AMLJSSFeatureEngineering
    , AMLJSSMaxAutoMLJobRuntimeReached
    , AMLJSSMaxCandidatesReached
    , AMLJSSModelTuning
    , AMLJSSStarting
    , AMLJSSStopped
    , AMLJSSStopping
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AutoMLJobSecondaryStatus = AutoMLJobSecondaryStatus' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern AMLJSSAnalyzingData :: AutoMLJobSecondaryStatus
pattern AMLJSSAnalyzingData = AutoMLJobSecondaryStatus' "AnalyzingData"

pattern AMLJSSCandidateDefinitionsGenerated :: AutoMLJobSecondaryStatus
pattern AMLJSSCandidateDefinitionsGenerated = AutoMLJobSecondaryStatus' "CandidateDefinitionsGenerated"

pattern AMLJSSFailed :: AutoMLJobSecondaryStatus
pattern AMLJSSFailed = AutoMLJobSecondaryStatus' "Failed"

pattern AMLJSSFeatureEngineering :: AutoMLJobSecondaryStatus
pattern AMLJSSFeatureEngineering = AutoMLJobSecondaryStatus' "FeatureEngineering"

pattern AMLJSSMaxAutoMLJobRuntimeReached :: AutoMLJobSecondaryStatus
pattern AMLJSSMaxAutoMLJobRuntimeReached = AutoMLJobSecondaryStatus' "MaxAutoMLJobRuntimeReached"

pattern AMLJSSMaxCandidatesReached :: AutoMLJobSecondaryStatus
pattern AMLJSSMaxCandidatesReached = AutoMLJobSecondaryStatus' "MaxCandidatesReached"

pattern AMLJSSModelTuning :: AutoMLJobSecondaryStatus
pattern AMLJSSModelTuning = AutoMLJobSecondaryStatus' "ModelTuning"

pattern AMLJSSStarting :: AutoMLJobSecondaryStatus
pattern AMLJSSStarting = AutoMLJobSecondaryStatus' "Starting"

pattern AMLJSSStopped :: AutoMLJobSecondaryStatus
pattern AMLJSSStopped = AutoMLJobSecondaryStatus' "Stopped"

pattern AMLJSSStopping :: AutoMLJobSecondaryStatus
pattern AMLJSSStopping = AutoMLJobSecondaryStatus' "Stopping"

{-# COMPLETE
  AMLJSSAnalyzingData,
  AMLJSSCandidateDefinitionsGenerated,
  AMLJSSFailed,
  AMLJSSFeatureEngineering,
  AMLJSSMaxAutoMLJobRuntimeReached,
  AMLJSSMaxCandidatesReached,
  AMLJSSModelTuning,
  AMLJSSStarting,
  AMLJSSStopped,
  AMLJSSStopping,
  AutoMLJobSecondaryStatus' #-}

instance FromText AutoMLJobSecondaryStatus where
    parser = (AutoMLJobSecondaryStatus' . mk) <$> takeText

instance ToText AutoMLJobSecondaryStatus where
    toText (AutoMLJobSecondaryStatus' ci) = original ci

-- | Represents an enum of /known/ $AutoMLJobSecondaryStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AutoMLJobSecondaryStatus where
    toEnum i = case i of
        0 -> AMLJSSAnalyzingData
        1 -> AMLJSSCandidateDefinitionsGenerated
        2 -> AMLJSSFailed
        3 -> AMLJSSFeatureEngineering
        4 -> AMLJSSMaxAutoMLJobRuntimeReached
        5 -> AMLJSSMaxCandidatesReached
        6 -> AMLJSSModelTuning
        7 -> AMLJSSStarting
        8 -> AMLJSSStopped
        9 -> AMLJSSStopping
        _ -> (error . showText) $ "Unknown index for AutoMLJobSecondaryStatus: " <> toText i
    fromEnum x = case x of
        AMLJSSAnalyzingData -> 0
        AMLJSSCandidateDefinitionsGenerated -> 1
        AMLJSSFailed -> 2
        AMLJSSFeatureEngineering -> 3
        AMLJSSMaxAutoMLJobRuntimeReached -> 4
        AMLJSSMaxCandidatesReached -> 5
        AMLJSSModelTuning -> 6
        AMLJSSStarting -> 7
        AMLJSSStopped -> 8
        AMLJSSStopping -> 9
        AutoMLJobSecondaryStatus' name -> (error . showText) $ "Unknown AutoMLJobSecondaryStatus: " <> original name

-- | Represents the bounds of /known/ $AutoMLJobSecondaryStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AutoMLJobSecondaryStatus where
    minBound = AMLJSSAnalyzingData
    maxBound = AMLJSSStopping

instance Hashable     AutoMLJobSecondaryStatus
instance NFData       AutoMLJobSecondaryStatus
instance ToByteString AutoMLJobSecondaryStatus
instance ToQuery      AutoMLJobSecondaryStatus
instance ToHeader     AutoMLJobSecondaryStatus

instance FromJSON AutoMLJobSecondaryStatus where
    parseJSON = parseJSONText "AutoMLJobSecondaryStatus"
