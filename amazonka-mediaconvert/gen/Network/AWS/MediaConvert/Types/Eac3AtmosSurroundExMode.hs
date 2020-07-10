{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosSurroundExMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Eac3AtmosSurroundExMode (
  Eac3AtmosSurroundExMode (
    ..
    , EASEMDisabled
    , EASEMEnabled
    , EASEMNotIndicated
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Specify whether your input audio has an additional center rear surround channel matrix encoded into your left and right surround channels.
data Eac3AtmosSurroundExMode = Eac3AtmosSurroundExMode' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern EASEMDisabled :: Eac3AtmosSurroundExMode
pattern EASEMDisabled = Eac3AtmosSurroundExMode' "DISABLED"

pattern EASEMEnabled :: Eac3AtmosSurroundExMode
pattern EASEMEnabled = Eac3AtmosSurroundExMode' "ENABLED"

pattern EASEMNotIndicated :: Eac3AtmosSurroundExMode
pattern EASEMNotIndicated = Eac3AtmosSurroundExMode' "NOT_INDICATED"

{-# COMPLETE
  EASEMDisabled,
  EASEMEnabled,
  EASEMNotIndicated,
  Eac3AtmosSurroundExMode' #-}

instance FromText Eac3AtmosSurroundExMode where
    parser = (Eac3AtmosSurroundExMode' . mk) <$> takeText

instance ToText Eac3AtmosSurroundExMode where
    toText (Eac3AtmosSurroundExMode' ci) = original ci

-- | Represents an enum of /known/ $Eac3AtmosSurroundExMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Eac3AtmosSurroundExMode where
    toEnum i = case i of
        0 -> EASEMDisabled
        1 -> EASEMEnabled
        2 -> EASEMNotIndicated
        _ -> (error . showText) $ "Unknown index for Eac3AtmosSurroundExMode: " <> toText i
    fromEnum x = case x of
        EASEMDisabled -> 0
        EASEMEnabled -> 1
        EASEMNotIndicated -> 2
        Eac3AtmosSurroundExMode' name -> (error . showText) $ "Unknown Eac3AtmosSurroundExMode: " <> original name

-- | Represents the bounds of /known/ $Eac3AtmosSurroundExMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Eac3AtmosSurroundExMode where
    minBound = EASEMDisabled
    maxBound = EASEMNotIndicated

instance Hashable     Eac3AtmosSurroundExMode
instance NFData       Eac3AtmosSurroundExMode
instance ToByteString Eac3AtmosSurroundExMode
instance ToQuery      Eac3AtmosSurroundExMode
instance ToHeader     Eac3AtmosSurroundExMode

instance ToJSON Eac3AtmosSurroundExMode where
    toJSON = toJSONText

instance FromJSON Eac3AtmosSurroundExMode where
    parseJSON = parseJSONText "Eac3AtmosSurroundExMode"
