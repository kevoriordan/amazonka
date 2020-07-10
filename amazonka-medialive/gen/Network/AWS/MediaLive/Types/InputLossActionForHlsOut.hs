{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputLossActionForHlsOut
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.InputLossActionForHlsOut (
  InputLossActionForHlsOut (
    ..
    , ILAFHOEmitOutput
    , ILAFHOPauseOutput
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Input Loss Action For Hls Out
data InputLossActionForHlsOut = InputLossActionForHlsOut' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern ILAFHOEmitOutput :: InputLossActionForHlsOut
pattern ILAFHOEmitOutput = InputLossActionForHlsOut' "EMIT_OUTPUT"

pattern ILAFHOPauseOutput :: InputLossActionForHlsOut
pattern ILAFHOPauseOutput = InputLossActionForHlsOut' "PAUSE_OUTPUT"

{-# COMPLETE
  ILAFHOEmitOutput,
  ILAFHOPauseOutput,
  InputLossActionForHlsOut' #-}

instance FromText InputLossActionForHlsOut where
    parser = (InputLossActionForHlsOut' . mk) <$> takeText

instance ToText InputLossActionForHlsOut where
    toText (InputLossActionForHlsOut' ci) = original ci

-- | Represents an enum of /known/ $InputLossActionForHlsOut.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InputLossActionForHlsOut where
    toEnum i = case i of
        0 -> ILAFHOEmitOutput
        1 -> ILAFHOPauseOutput
        _ -> (error . showText) $ "Unknown index for InputLossActionForHlsOut: " <> toText i
    fromEnum x = case x of
        ILAFHOEmitOutput -> 0
        ILAFHOPauseOutput -> 1
        InputLossActionForHlsOut' name -> (error . showText) $ "Unknown InputLossActionForHlsOut: " <> original name

-- | Represents the bounds of /known/ $InputLossActionForHlsOut.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InputLossActionForHlsOut where
    minBound = ILAFHOEmitOutput
    maxBound = ILAFHOPauseOutput

instance Hashable     InputLossActionForHlsOut
instance NFData       InputLossActionForHlsOut
instance ToByteString InputLossActionForHlsOut
instance ToQuery      InputLossActionForHlsOut
instance ToHeader     InputLossActionForHlsOut

instance ToJSON InputLossActionForHlsOut where
    toJSON = toJSONText

instance FromJSON InputLossActionForHlsOut where
    parseJSON = parseJSONText "InputLossActionForHlsOut"
