{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTuningJobWarmStartType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.HyperParameterTuningJobWarmStartType (
  HyperParameterTuningJobWarmStartType (
    ..
    , IdenticalDataAndAlgorithm
    , TransferLearning
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data HyperParameterTuningJobWarmStartType = HyperParameterTuningJobWarmStartType' (CI
                                                                                     Text)
                                              deriving (Eq, Ord, Read, Show,
                                                        Data, Typeable, Generic)

pattern IdenticalDataAndAlgorithm :: HyperParameterTuningJobWarmStartType
pattern IdenticalDataAndAlgorithm = HyperParameterTuningJobWarmStartType' "IdenticalDataAndAlgorithm"

pattern TransferLearning :: HyperParameterTuningJobWarmStartType
pattern TransferLearning = HyperParameterTuningJobWarmStartType' "TransferLearning"

{-# COMPLETE
  IdenticalDataAndAlgorithm,
  TransferLearning,
  HyperParameterTuningJobWarmStartType' #-}

instance FromText HyperParameterTuningJobWarmStartType where
    parser = (HyperParameterTuningJobWarmStartType' . mk) <$> takeText

instance ToText HyperParameterTuningJobWarmStartType where
    toText (HyperParameterTuningJobWarmStartType' ci) = original ci

-- | Represents an enum of /known/ $HyperParameterTuningJobWarmStartType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HyperParameterTuningJobWarmStartType where
    toEnum i = case i of
        0 -> IdenticalDataAndAlgorithm
        1 -> TransferLearning
        _ -> (error . showText) $ "Unknown index for HyperParameterTuningJobWarmStartType: " <> toText i
    fromEnum x = case x of
        IdenticalDataAndAlgorithm -> 0
        TransferLearning -> 1
        HyperParameterTuningJobWarmStartType' name -> (error . showText) $ "Unknown HyperParameterTuningJobWarmStartType: " <> original name

-- | Represents the bounds of /known/ $HyperParameterTuningJobWarmStartType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HyperParameterTuningJobWarmStartType where
    minBound = IdenticalDataAndAlgorithm
    maxBound = TransferLearning

instance Hashable     HyperParameterTuningJobWarmStartType
instance NFData       HyperParameterTuningJobWarmStartType
instance ToByteString HyperParameterTuningJobWarmStartType
instance ToQuery      HyperParameterTuningJobWarmStartType
instance ToHeader     HyperParameterTuningJobWarmStartType

instance ToJSON HyperParameterTuningJobWarmStartType where
    toJSON = toJSONText

instance FromJSON HyperParameterTuningJobWarmStartType where
    parseJSON = parseJSONText "HyperParameterTuningJobWarmStartType"
