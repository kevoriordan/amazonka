{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrainingJobEarlyStoppingType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.TrainingJobEarlyStoppingType (
  TrainingJobEarlyStoppingType (
    ..
    , TJESTAuto
    , TJESTOff
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TrainingJobEarlyStoppingType = TrainingJobEarlyStoppingType' (CI
                                                                     Text)
                                      deriving (Eq, Ord, Read, Show, Data,
                                                Typeable, Generic)

pattern TJESTAuto :: TrainingJobEarlyStoppingType
pattern TJESTAuto = TrainingJobEarlyStoppingType' "Auto"

pattern TJESTOff :: TrainingJobEarlyStoppingType
pattern TJESTOff = TrainingJobEarlyStoppingType' "Off"

{-# COMPLETE
  TJESTAuto,
  TJESTOff,
  TrainingJobEarlyStoppingType' #-}

instance FromText TrainingJobEarlyStoppingType where
    parser = (TrainingJobEarlyStoppingType' . mk) <$> takeText

instance ToText TrainingJobEarlyStoppingType where
    toText (TrainingJobEarlyStoppingType' ci) = original ci

-- | Represents an enum of /known/ $TrainingJobEarlyStoppingType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TrainingJobEarlyStoppingType where
    toEnum i = case i of
        0 -> TJESTAuto
        1 -> TJESTOff
        _ -> (error . showText) $ "Unknown index for TrainingJobEarlyStoppingType: " <> toText i
    fromEnum x = case x of
        TJESTAuto -> 0
        TJESTOff -> 1
        TrainingJobEarlyStoppingType' name -> (error . showText) $ "Unknown TrainingJobEarlyStoppingType: " <> original name

-- | Represents the bounds of /known/ $TrainingJobEarlyStoppingType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TrainingJobEarlyStoppingType where
    minBound = TJESTAuto
    maxBound = TJESTOff

instance Hashable     TrainingJobEarlyStoppingType
instance NFData       TrainingJobEarlyStoppingType
instance ToByteString TrainingJobEarlyStoppingType
instance ToQuery      TrainingJobEarlyStoppingType
instance ToHeader     TrainingJobEarlyStoppingType

instance ToJSON TrainingJobEarlyStoppingType where
    toJSON = toJSONText

instance FromJSON TrainingJobEarlyStoppingType where
    parseJSON = parseJSONText "TrainingJobEarlyStoppingType"
