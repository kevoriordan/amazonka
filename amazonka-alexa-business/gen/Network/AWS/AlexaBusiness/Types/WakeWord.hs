{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.WakeWord
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.WakeWord (
  WakeWord (
    ..
    , Alexa
    , Amazon
    , Computer
    , Echo
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data WakeWord = WakeWord' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern Alexa :: WakeWord
pattern Alexa = WakeWord' "ALEXA"

pattern Amazon :: WakeWord
pattern Amazon = WakeWord' "AMAZON"

pattern Computer :: WakeWord
pattern Computer = WakeWord' "COMPUTER"

pattern Echo :: WakeWord
pattern Echo = WakeWord' "ECHO"

{-# COMPLETE
  Alexa,
  Amazon,
  Computer,
  Echo,
  WakeWord' #-}

instance FromText WakeWord where
    parser = (WakeWord' . mk) <$> takeText

instance ToText WakeWord where
    toText (WakeWord' ci) = original ci

-- | Represents an enum of /known/ $WakeWord.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum WakeWord where
    toEnum i = case i of
        0 -> Alexa
        1 -> Amazon
        2 -> Computer
        3 -> Echo
        _ -> (error . showText) $ "Unknown index for WakeWord: " <> toText i
    fromEnum x = case x of
        Alexa -> 0
        Amazon -> 1
        Computer -> 2
        Echo -> 3
        WakeWord' name -> (error . showText) $ "Unknown WakeWord: " <> original name

-- | Represents the bounds of /known/ $WakeWord.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded WakeWord where
    minBound = Alexa
    maxBound = Echo

instance Hashable     WakeWord
instance NFData       WakeWord
instance ToByteString WakeWord
instance ToQuery      WakeWord
instance ToHeader     WakeWord

instance ToJSON WakeWord where
    toJSON = toJSONText

instance FromJSON WakeWord where
    parseJSON = parseJSONText "WakeWord"
