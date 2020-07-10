{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.StackAttributesKeys
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.StackAttributesKeys (
  StackAttributesKeys (
    ..
    , Color
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StackAttributesKeys = StackAttributesKeys' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern Color :: StackAttributesKeys
pattern Color = StackAttributesKeys' "Color"

{-# COMPLETE
  Color,
  StackAttributesKeys' #-}

instance FromText StackAttributesKeys where
    parser = (StackAttributesKeys' . mk) <$> takeText

instance ToText StackAttributesKeys where
    toText (StackAttributesKeys' ci) = original ci

-- | Represents an enum of /known/ $StackAttributesKeys.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StackAttributesKeys where
    toEnum i = case i of
        0 -> Color
        _ -> (error . showText) $ "Unknown index for StackAttributesKeys: " <> toText i
    fromEnum x = case x of
        Color -> 0
        StackAttributesKeys' name -> (error . showText) $ "Unknown StackAttributesKeys: " <> original name

-- | Represents the bounds of /known/ $StackAttributesKeys.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StackAttributesKeys where
    minBound = Color
    maxBound = Color

instance Hashable     StackAttributesKeys
instance NFData       StackAttributesKeys
instance ToByteString StackAttributesKeys
instance ToQuery      StackAttributesKeys
instance ToHeader     StackAttributesKeys

instance ToJSON StackAttributesKeys where
    toJSON = toJSONText

instance FromJSON StackAttributesKeys where
    parseJSON = parseJSONText "StackAttributesKeys"
