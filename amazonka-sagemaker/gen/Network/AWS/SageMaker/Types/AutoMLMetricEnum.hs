{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLMetricEnum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AutoMLMetricEnum (
  AutoMLMetricEnum (
    ..
    , Accuracy
    , F1
    , F1macro
    , Mse
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AutoMLMetricEnum = AutoMLMetricEnum' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern Accuracy :: AutoMLMetricEnum
pattern Accuracy = AutoMLMetricEnum' "Accuracy"

pattern F1 :: AutoMLMetricEnum
pattern F1 = AutoMLMetricEnum' "F1"

pattern F1macro :: AutoMLMetricEnum
pattern F1macro = AutoMLMetricEnum' "F1macro"

pattern Mse :: AutoMLMetricEnum
pattern Mse = AutoMLMetricEnum' "MSE"

{-# COMPLETE
  Accuracy,
  F1,
  F1macro,
  Mse,
  AutoMLMetricEnum' #-}

instance FromText AutoMLMetricEnum where
    parser = (AutoMLMetricEnum' . mk) <$> takeText

instance ToText AutoMLMetricEnum where
    toText (AutoMLMetricEnum' ci) = original ci

-- | Represents an enum of /known/ $AutoMLMetricEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AutoMLMetricEnum where
    toEnum i = case i of
        0 -> Accuracy
        1 -> F1
        2 -> F1macro
        3 -> Mse
        _ -> (error . showText) $ "Unknown index for AutoMLMetricEnum: " <> toText i
    fromEnum x = case x of
        Accuracy -> 0
        F1 -> 1
        F1macro -> 2
        Mse -> 3
        AutoMLMetricEnum' name -> (error . showText) $ "Unknown AutoMLMetricEnum: " <> original name

-- | Represents the bounds of /known/ $AutoMLMetricEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AutoMLMetricEnum where
    minBound = Accuracy
    maxBound = Mse

instance Hashable     AutoMLMetricEnum
instance NFData       AutoMLMetricEnum
instance ToByteString AutoMLMetricEnum
instance ToQuery      AutoMLMetricEnum
instance ToHeader     AutoMLMetricEnum

instance ToJSON AutoMLMetricEnum where
    toJSON = toJSONText

instance FromJSON AutoMLMetricEnum where
    parseJSON = parseJSONText "AutoMLMetricEnum"
