{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTuningJobObjectiveType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.HyperParameterTuningJobObjectiveType (
  HyperParameterTuningJobObjectiveType (
    ..
    , Maximize
    , Minimize
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data HyperParameterTuningJobObjectiveType = HyperParameterTuningJobObjectiveType' (CI
                                                                                     Text)
                                              deriving (Eq, Ord, Read, Show,
                                                        Data, Typeable, Generic)

pattern Maximize :: HyperParameterTuningJobObjectiveType
pattern Maximize = HyperParameterTuningJobObjectiveType' "Maximize"

pattern Minimize :: HyperParameterTuningJobObjectiveType
pattern Minimize = HyperParameterTuningJobObjectiveType' "Minimize"

{-# COMPLETE
  Maximize,
  Minimize,
  HyperParameterTuningJobObjectiveType' #-}

instance FromText HyperParameterTuningJobObjectiveType where
    parser = (HyperParameterTuningJobObjectiveType' . mk) <$> takeText

instance ToText HyperParameterTuningJobObjectiveType where
    toText (HyperParameterTuningJobObjectiveType' ci) = original ci

-- | Represents an enum of /known/ $HyperParameterTuningJobObjectiveType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HyperParameterTuningJobObjectiveType where
    toEnum i = case i of
        0 -> Maximize
        1 -> Minimize
        _ -> (error . showText) $ "Unknown index for HyperParameterTuningJobObjectiveType: " <> toText i
    fromEnum x = case x of
        Maximize -> 0
        Minimize -> 1
        HyperParameterTuningJobObjectiveType' name -> (error . showText) $ "Unknown HyperParameterTuningJobObjectiveType: " <> original name

-- | Represents the bounds of /known/ $HyperParameterTuningJobObjectiveType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HyperParameterTuningJobObjectiveType where
    minBound = Maximize
    maxBound = Minimize

instance Hashable     HyperParameterTuningJobObjectiveType
instance NFData       HyperParameterTuningJobObjectiveType
instance ToByteString HyperParameterTuningJobObjectiveType
instance ToQuery      HyperParameterTuningJobObjectiveType
instance ToHeader     HyperParameterTuningJobObjectiveType

instance ToJSON HyperParameterTuningJobObjectiveType where
    toJSON = toJSONText

instance FromJSON HyperParameterTuningJobObjectiveType where
    parseJSON = parseJSONText "HyperParameterTuningJobObjectiveType"
