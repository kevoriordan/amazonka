{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DolbyVisionLevel6Mode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.DolbyVisionLevel6Mode (
  DolbyVisionLevel6Mode (
    ..
    , Passthrough
    , Recalculate
    , Specify
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Use Dolby Vision Mode to choose how the service will handle Dolby Vision MaxCLL and MaxFALL properies.
data DolbyVisionLevel6Mode = DolbyVisionLevel6Mode' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern Passthrough :: DolbyVisionLevel6Mode
pattern Passthrough = DolbyVisionLevel6Mode' "PASSTHROUGH"

pattern Recalculate :: DolbyVisionLevel6Mode
pattern Recalculate = DolbyVisionLevel6Mode' "RECALCULATE"

pattern Specify :: DolbyVisionLevel6Mode
pattern Specify = DolbyVisionLevel6Mode' "SPECIFY"

{-# COMPLETE
  Passthrough,
  Recalculate,
  Specify,
  DolbyVisionLevel6Mode' #-}

instance FromText DolbyVisionLevel6Mode where
    parser = (DolbyVisionLevel6Mode' . mk) <$> takeText

instance ToText DolbyVisionLevel6Mode where
    toText (DolbyVisionLevel6Mode' ci) = original ci

-- | Represents an enum of /known/ $DolbyVisionLevel6Mode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DolbyVisionLevel6Mode where
    toEnum i = case i of
        0 -> Passthrough
        1 -> Recalculate
        2 -> Specify
        _ -> (error . showText) $ "Unknown index for DolbyVisionLevel6Mode: " <> toText i
    fromEnum x = case x of
        Passthrough -> 0
        Recalculate -> 1
        Specify -> 2
        DolbyVisionLevel6Mode' name -> (error . showText) $ "Unknown DolbyVisionLevel6Mode: " <> original name

-- | Represents the bounds of /known/ $DolbyVisionLevel6Mode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DolbyVisionLevel6Mode where
    minBound = Passthrough
    maxBound = Specify

instance Hashable     DolbyVisionLevel6Mode
instance NFData       DolbyVisionLevel6Mode
instance ToByteString DolbyVisionLevel6Mode
instance ToQuery      DolbyVisionLevel6Mode
instance ToHeader     DolbyVisionLevel6Mode

instance ToJSON DolbyVisionLevel6Mode where
    toJSON = toJSONText

instance FromJSON DolbyVisionLevel6Mode where
    parseJSON = parseJSONText "DolbyVisionLevel6Mode"
