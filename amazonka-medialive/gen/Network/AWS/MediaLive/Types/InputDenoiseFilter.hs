{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDenoiseFilter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.InputDenoiseFilter (
  InputDenoiseFilter (
    ..
    , IDisabled
    , IEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Input Denoise Filter
data InputDenoiseFilter = InputDenoiseFilter' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern IDisabled :: InputDenoiseFilter
pattern IDisabled = InputDenoiseFilter' "DISABLED"

pattern IEnabled :: InputDenoiseFilter
pattern IEnabled = InputDenoiseFilter' "ENABLED"

{-# COMPLETE
  IDisabled,
  IEnabled,
  InputDenoiseFilter' #-}

instance FromText InputDenoiseFilter where
    parser = (InputDenoiseFilter' . mk) <$> takeText

instance ToText InputDenoiseFilter where
    toText (InputDenoiseFilter' ci) = original ci

-- | Represents an enum of /known/ $InputDenoiseFilter.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InputDenoiseFilter where
    toEnum i = case i of
        0 -> IDisabled
        1 -> IEnabled
        _ -> (error . showText) $ "Unknown index for InputDenoiseFilter: " <> toText i
    fromEnum x = case x of
        IDisabled -> 0
        IEnabled -> 1
        InputDenoiseFilter' name -> (error . showText) $ "Unknown InputDenoiseFilter: " <> original name

-- | Represents the bounds of /known/ $InputDenoiseFilter.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InputDenoiseFilter where
    minBound = IDisabled
    maxBound = IEnabled

instance Hashable     InputDenoiseFilter
instance NFData       InputDenoiseFilter
instance ToByteString InputDenoiseFilter
instance ToQuery      InputDenoiseFilter
instance ToHeader     InputDenoiseFilter

instance ToJSON InputDenoiseFilter where
    toJSON = toJSONText

instance FromJSON InputDenoiseFilter where
    parseJSON = parseJSONText "InputDenoiseFilter"
