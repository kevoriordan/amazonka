{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.TemporalFilterPostFilterSharpening
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.TemporalFilterPostFilterSharpening (
  TemporalFilterPostFilterSharpening (
    ..
    , TFPFSAuto
    , TFPFSDisabled
    , TFPFSEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Temporal Filter Post Filter Sharpening
data TemporalFilterPostFilterSharpening = TemporalFilterPostFilterSharpening' (CI
                                                                                 Text)
                                            deriving (Eq, Ord, Read, Show, Data,
                                                      Typeable, Generic)

pattern TFPFSAuto :: TemporalFilterPostFilterSharpening
pattern TFPFSAuto = TemporalFilterPostFilterSharpening' "AUTO"

pattern TFPFSDisabled :: TemporalFilterPostFilterSharpening
pattern TFPFSDisabled = TemporalFilterPostFilterSharpening' "DISABLED"

pattern TFPFSEnabled :: TemporalFilterPostFilterSharpening
pattern TFPFSEnabled = TemporalFilterPostFilterSharpening' "ENABLED"

{-# COMPLETE
  TFPFSAuto,
  TFPFSDisabled,
  TFPFSEnabled,
  TemporalFilterPostFilterSharpening' #-}

instance FromText TemporalFilterPostFilterSharpening where
    parser = (TemporalFilterPostFilterSharpening' . mk) <$> takeText

instance ToText TemporalFilterPostFilterSharpening where
    toText (TemporalFilterPostFilterSharpening' ci) = original ci

-- | Represents an enum of /known/ $TemporalFilterPostFilterSharpening.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TemporalFilterPostFilterSharpening where
    toEnum i = case i of
        0 -> TFPFSAuto
        1 -> TFPFSDisabled
        2 -> TFPFSEnabled
        _ -> (error . showText) $ "Unknown index for TemporalFilterPostFilterSharpening: " <> toText i
    fromEnum x = case x of
        TFPFSAuto -> 0
        TFPFSDisabled -> 1
        TFPFSEnabled -> 2
        TemporalFilterPostFilterSharpening' name -> (error . showText) $ "Unknown TemporalFilterPostFilterSharpening: " <> original name

-- | Represents the bounds of /known/ $TemporalFilterPostFilterSharpening.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TemporalFilterPostFilterSharpening where
    minBound = TFPFSAuto
    maxBound = TFPFSEnabled

instance Hashable     TemporalFilterPostFilterSharpening
instance NFData       TemporalFilterPostFilterSharpening
instance ToByteString TemporalFilterPostFilterSharpening
instance ToQuery      TemporalFilterPostFilterSharpening
instance ToHeader     TemporalFilterPostFilterSharpening

instance ToJSON TemporalFilterPostFilterSharpening where
    toJSON = toJSONText

instance FromJSON TemporalFilterPostFilterSharpening where
    parseJSON = parseJSONText "TemporalFilterPostFilterSharpening"
