{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosBitstreamMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Eac3AtmosBitstreamMode (
  Eac3AtmosBitstreamMode (
    ..
    , CompleteMain
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Specify the bitstream mode for the E-AC-3 stream that the encoder emits. For more information about the EAC3 bitstream mode, see ATSC A/52-2012 (Annex E).
data Eac3AtmosBitstreamMode = Eac3AtmosBitstreamMode' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern CompleteMain :: Eac3AtmosBitstreamMode
pattern CompleteMain = Eac3AtmosBitstreamMode' "COMPLETE_MAIN"

{-# COMPLETE
  CompleteMain,
  Eac3AtmosBitstreamMode' #-}

instance FromText Eac3AtmosBitstreamMode where
    parser = (Eac3AtmosBitstreamMode' . mk) <$> takeText

instance ToText Eac3AtmosBitstreamMode where
    toText (Eac3AtmosBitstreamMode' ci) = original ci

-- | Represents an enum of /known/ $Eac3AtmosBitstreamMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Eac3AtmosBitstreamMode where
    toEnum i = case i of
        0 -> CompleteMain
        _ -> (error . showText) $ "Unknown index for Eac3AtmosBitstreamMode: " <> toText i
    fromEnum x = case x of
        CompleteMain -> 0
        Eac3AtmosBitstreamMode' name -> (error . showText) $ "Unknown Eac3AtmosBitstreamMode: " <> original name

-- | Represents the bounds of /known/ $Eac3AtmosBitstreamMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Eac3AtmosBitstreamMode where
    minBound = CompleteMain
    maxBound = CompleteMain

instance Hashable     Eac3AtmosBitstreamMode
instance NFData       Eac3AtmosBitstreamMode
instance ToByteString Eac3AtmosBitstreamMode
instance ToQuery      Eac3AtmosBitstreamMode
instance ToHeader     Eac3AtmosBitstreamMode

instance ToJSON Eac3AtmosBitstreamMode where
    toJSON = toJSONText

instance FromJSON Eac3AtmosBitstreamMode where
    parseJSON = parseJSONText "Eac3AtmosBitstreamMode"
