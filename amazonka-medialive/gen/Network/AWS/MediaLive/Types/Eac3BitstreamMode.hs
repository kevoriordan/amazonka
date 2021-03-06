{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Eac3BitstreamMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Eac3BitstreamMode (
  Eac3BitstreamMode (
    ..
    , EBMCommentary
    , EBMCompleteMain
    , EBMEmergency
    , EBMHearingImpaired
    , EBMVisuallyImpaired
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Eac3 Bitstream Mode
data Eac3BitstreamMode = Eac3BitstreamMode' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern EBMCommentary :: Eac3BitstreamMode
pattern EBMCommentary = Eac3BitstreamMode' "COMMENTARY"

pattern EBMCompleteMain :: Eac3BitstreamMode
pattern EBMCompleteMain = Eac3BitstreamMode' "COMPLETE_MAIN"

pattern EBMEmergency :: Eac3BitstreamMode
pattern EBMEmergency = Eac3BitstreamMode' "EMERGENCY"

pattern EBMHearingImpaired :: Eac3BitstreamMode
pattern EBMHearingImpaired = Eac3BitstreamMode' "HEARING_IMPAIRED"

pattern EBMVisuallyImpaired :: Eac3BitstreamMode
pattern EBMVisuallyImpaired = Eac3BitstreamMode' "VISUALLY_IMPAIRED"

{-# COMPLETE
  EBMCommentary,
  EBMCompleteMain,
  EBMEmergency,
  EBMHearingImpaired,
  EBMVisuallyImpaired,
  Eac3BitstreamMode' #-}

instance FromText Eac3BitstreamMode where
    parser = (Eac3BitstreamMode' . mk) <$> takeText

instance ToText Eac3BitstreamMode where
    toText (Eac3BitstreamMode' ci) = original ci

-- | Represents an enum of /known/ $Eac3BitstreamMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Eac3BitstreamMode where
    toEnum i = case i of
        0 -> EBMCommentary
        1 -> EBMCompleteMain
        2 -> EBMEmergency
        3 -> EBMHearingImpaired
        4 -> EBMVisuallyImpaired
        _ -> (error . showText) $ "Unknown index for Eac3BitstreamMode: " <> toText i
    fromEnum x = case x of
        EBMCommentary -> 0
        EBMCompleteMain -> 1
        EBMEmergency -> 2
        EBMHearingImpaired -> 3
        EBMVisuallyImpaired -> 4
        Eac3BitstreamMode' name -> (error . showText) $ "Unknown Eac3BitstreamMode: " <> original name

-- | Represents the bounds of /known/ $Eac3BitstreamMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Eac3BitstreamMode where
    minBound = EBMCommentary
    maxBound = EBMVisuallyImpaired

instance Hashable     Eac3BitstreamMode
instance NFData       Eac3BitstreamMode
instance ToByteString Eac3BitstreamMode
instance ToQuery      Eac3BitstreamMode
instance ToHeader     Eac3BitstreamMode

instance ToJSON Eac3BitstreamMode where
    toJSON = toJSONText

instance FromJSON Eac3BitstreamMode where
    parseJSON = parseJSONText "Eac3BitstreamMode"
