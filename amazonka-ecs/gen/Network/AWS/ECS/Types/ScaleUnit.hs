{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ScaleUnit
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.ScaleUnit (
  ScaleUnit (
    ..
    , Percent
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ScaleUnit = ScaleUnit' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern Percent :: ScaleUnit
pattern Percent = ScaleUnit' "PERCENT"

{-# COMPLETE
  Percent,
  ScaleUnit' #-}

instance FromText ScaleUnit where
    parser = (ScaleUnit' . mk) <$> takeText

instance ToText ScaleUnit where
    toText (ScaleUnit' ci) = original ci

-- | Represents an enum of /known/ $ScaleUnit.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ScaleUnit where
    toEnum i = case i of
        0 -> Percent
        _ -> (error . showText) $ "Unknown index for ScaleUnit: " <> toText i
    fromEnum x = case x of
        Percent -> 0
        ScaleUnit' name -> (error . showText) $ "Unknown ScaleUnit: " <> original name

-- | Represents the bounds of /known/ $ScaleUnit.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ScaleUnit where
    minBound = Percent
    maxBound = Percent

instance Hashable     ScaleUnit
instance NFData       ScaleUnit
instance ToByteString ScaleUnit
instance ToQuery      ScaleUnit
instance ToHeader     ScaleUnit

instance ToJSON ScaleUnit where
    toJSON = toJSONText

instance FromJSON ScaleUnit where
    parseJSON = parseJSONText "ScaleUnit"
