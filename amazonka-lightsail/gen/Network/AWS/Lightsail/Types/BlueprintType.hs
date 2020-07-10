{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.BlueprintType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.BlueprintType (
  BlueprintType (
    ..
    , App
    , OS
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data BlueprintType = BlueprintType' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern App :: BlueprintType
pattern App = BlueprintType' "app"

pattern OS :: BlueprintType
pattern OS = BlueprintType' "os"

{-# COMPLETE
  App,
  OS,
  BlueprintType' #-}

instance FromText BlueprintType where
    parser = (BlueprintType' . mk) <$> takeText

instance ToText BlueprintType where
    toText (BlueprintType' ci) = original ci

-- | Represents an enum of /known/ $BlueprintType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BlueprintType where
    toEnum i = case i of
        0 -> App
        1 -> OS
        _ -> (error . showText) $ "Unknown index for BlueprintType: " <> toText i
    fromEnum x = case x of
        App -> 0
        OS -> 1
        BlueprintType' name -> (error . showText) $ "Unknown BlueprintType: " <> original name

-- | Represents the bounds of /known/ $BlueprintType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BlueprintType where
    minBound = App
    maxBound = OS

instance Hashable     BlueprintType
instance NFData       BlueprintType
instance ToByteString BlueprintType
instance ToQuery      BlueprintType
instance ToHeader     BlueprintType

instance FromJSON BlueprintType where
    parseJSON = parseJSONText "BlueprintType"
