{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ParameterType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ParameterType (
  ParameterType (
    ..
    , PTCategorical
    , PTContinuous
    , PTFreeText
    , PTInteger
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ParameterType = ParameterType' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern PTCategorical :: ParameterType
pattern PTCategorical = ParameterType' "Categorical"

pattern PTContinuous :: ParameterType
pattern PTContinuous = ParameterType' "Continuous"

pattern PTFreeText :: ParameterType
pattern PTFreeText = ParameterType' "FreeText"

pattern PTInteger :: ParameterType
pattern PTInteger = ParameterType' "Integer"

{-# COMPLETE
  PTCategorical,
  PTContinuous,
  PTFreeText,
  PTInteger,
  ParameterType' #-}

instance FromText ParameterType where
    parser = (ParameterType' . mk) <$> takeText

instance ToText ParameterType where
    toText (ParameterType' ci) = original ci

-- | Represents an enum of /known/ $ParameterType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ParameterType where
    toEnum i = case i of
        0 -> PTCategorical
        1 -> PTContinuous
        2 -> PTFreeText
        3 -> PTInteger
        _ -> (error . showText) $ "Unknown index for ParameterType: " <> toText i
    fromEnum x = case x of
        PTCategorical -> 0
        PTContinuous -> 1
        PTFreeText -> 2
        PTInteger -> 3
        ParameterType' name -> (error . showText) $ "Unknown ParameterType: " <> original name

-- | Represents the bounds of /known/ $ParameterType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ParameterType where
    minBound = PTCategorical
    maxBound = PTInteger

instance Hashable     ParameterType
instance NFData       ParameterType
instance ToByteString ParameterType
instance ToQuery      ParameterType
instance ToHeader     ParameterType

instance ToJSON ParameterType where
    toJSON = toJSONText

instance FromJSON ParameterType where
    parseJSON = parseJSONText "ParameterType"
