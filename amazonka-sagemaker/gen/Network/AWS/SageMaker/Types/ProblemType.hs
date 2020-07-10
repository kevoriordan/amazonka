{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProblemType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ProblemType (
  ProblemType (
    ..
    , BinaryClassification
    , MulticlassClassification
    , Regression
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ProblemType = ProblemType' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern BinaryClassification :: ProblemType
pattern BinaryClassification = ProblemType' "BinaryClassification"

pattern MulticlassClassification :: ProblemType
pattern MulticlassClassification = ProblemType' "MulticlassClassification"

pattern Regression :: ProblemType
pattern Regression = ProblemType' "Regression"

{-# COMPLETE
  BinaryClassification,
  MulticlassClassification,
  Regression,
  ProblemType' #-}

instance FromText ProblemType where
    parser = (ProblemType' . mk) <$> takeText

instance ToText ProblemType where
    toText (ProblemType' ci) = original ci

-- | Represents an enum of /known/ $ProblemType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProblemType where
    toEnum i = case i of
        0 -> BinaryClassification
        1 -> MulticlassClassification
        2 -> Regression
        _ -> (error . showText) $ "Unknown index for ProblemType: " <> toText i
    fromEnum x = case x of
        BinaryClassification -> 0
        MulticlassClassification -> 1
        Regression -> 2
        ProblemType' name -> (error . showText) $ "Unknown ProblemType: " <> original name

-- | Represents the bounds of /known/ $ProblemType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProblemType where
    minBound = BinaryClassification
    maxBound = Regression

instance Hashable     ProblemType
instance NFData       ProblemType
instance ToByteString ProblemType
instance ToQuery      ProblemType
instance ToHeader     ProblemType

instance ToJSON ProblemType where
    toJSON = toJSONText

instance FromJSON ProblemType where
    parseJSON = parseJSONText "ProblemType"
