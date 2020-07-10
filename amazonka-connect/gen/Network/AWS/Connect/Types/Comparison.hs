{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.Comparison
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.Comparison (
  Comparison (
    ..
    , LT'
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Comparison = Comparison' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern LT' :: Comparison
pattern LT' = Comparison' "LT"

{-# COMPLETE
  LT',
  Comparison' #-}

instance FromText Comparison where
    parser = (Comparison' . mk) <$> takeText

instance ToText Comparison where
    toText (Comparison' ci) = original ci

-- | Represents an enum of /known/ $Comparison.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Comparison where
    toEnum i = case i of
        0 -> LT'
        _ -> (error . showText) $ "Unknown index for Comparison: " <> toText i
    fromEnum x = case x of
        LT' -> 0
        Comparison' name -> (error . showText) $ "Unknown Comparison: " <> original name

-- | Represents the bounds of /known/ $Comparison.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Comparison where
    minBound = LT'
    maxBound = LT'

instance Hashable     Comparison
instance NFData       Comparison
instance ToByteString Comparison
instance ToQuery      Comparison
instance ToHeader     Comparison

instance ToJSON Comparison where
    toJSON = toJSONText

instance FromJSON Comparison where
    parseJSON = parseJSONText "Comparison"
