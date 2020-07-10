{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Ac3LfeFilter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Ac3LfeFilter (
  Ac3LfeFilter (
    ..
    , ALFDisabled
    , ALFEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with 3_2_LFE coding mode.
data Ac3LfeFilter = Ac3LfeFilter' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern ALFDisabled :: Ac3LfeFilter
pattern ALFDisabled = Ac3LfeFilter' "DISABLED"

pattern ALFEnabled :: Ac3LfeFilter
pattern ALFEnabled = Ac3LfeFilter' "ENABLED"

{-# COMPLETE
  ALFDisabled,
  ALFEnabled,
  Ac3LfeFilter' #-}

instance FromText Ac3LfeFilter where
    parser = (Ac3LfeFilter' . mk) <$> takeText

instance ToText Ac3LfeFilter where
    toText (Ac3LfeFilter' ci) = original ci

-- | Represents an enum of /known/ $Ac3LfeFilter.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Ac3LfeFilter where
    toEnum i = case i of
        0 -> ALFDisabled
        1 -> ALFEnabled
        _ -> (error . showText) $ "Unknown index for Ac3LfeFilter: " <> toText i
    fromEnum x = case x of
        ALFDisabled -> 0
        ALFEnabled -> 1
        Ac3LfeFilter' name -> (error . showText) $ "Unknown Ac3LfeFilter: " <> original name

-- | Represents the bounds of /known/ $Ac3LfeFilter.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Ac3LfeFilter where
    minBound = ALFDisabled
    maxBound = ALFEnabled

instance Hashable     Ac3LfeFilter
instance NFData       Ac3LfeFilter
instance ToByteString Ac3LfeFilter
instance ToQuery      Ac3LfeFilter
instance ToHeader     Ac3LfeFilter

instance ToJSON Ac3LfeFilter where
    toJSON = toJSONText

instance FromJSON Ac3LfeFilter where
    parseJSON = parseJSONText "Ac3LfeFilter"
