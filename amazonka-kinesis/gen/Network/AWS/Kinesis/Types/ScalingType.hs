{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.ScalingType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.ScalingType (
  ScalingType (
    ..
    , UniformScaling
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ScalingType = ScalingType' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern UniformScaling :: ScalingType
pattern UniformScaling = ScalingType' "UNIFORM_SCALING"

{-# COMPLETE
  UniformScaling,
  ScalingType' #-}

instance FromText ScalingType where
    parser = (ScalingType' . mk) <$> takeText

instance ToText ScalingType where
    toText (ScalingType' ci) = original ci

-- | Represents an enum of /known/ $ScalingType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ScalingType where
    toEnum i = case i of
        0 -> UniformScaling
        _ -> (error . showText) $ "Unknown index for ScalingType: " <> toText i
    fromEnum x = case x of
        UniformScaling -> 0
        ScalingType' name -> (error . showText) $ "Unknown ScalingType: " <> original name

-- | Represents the bounds of /known/ $ScalingType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ScalingType where
    minBound = UniformScaling
    maxBound = UniformScaling

instance Hashable     ScalingType
instance NFData       ScalingType
instance ToByteString ScalingType
instance ToQuery      ScalingType
instance ToHeader     ScalingType

instance ToJSON ScalingType where
    toJSON = toJSONText
