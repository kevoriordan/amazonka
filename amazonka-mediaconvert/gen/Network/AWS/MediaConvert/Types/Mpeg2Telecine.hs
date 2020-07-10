{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2Telecine
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Mpeg2Telecine (
  Mpeg2Telecine (
    ..
    , MTHard
    , MTNone
    , MTSoft
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Only use Telecine (Mpeg2Telecine) when you set Framerate (Framerate) to 29.970. Set Telecine (Mpeg2Telecine) to Hard (hard) to produce a 29.97i output from a 23.976 input. Set it to Soft (soft) to produce 23.976 output and leave converstion to the player.
data Mpeg2Telecine = Mpeg2Telecine' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern MTHard :: Mpeg2Telecine
pattern MTHard = Mpeg2Telecine' "HARD"

pattern MTNone :: Mpeg2Telecine
pattern MTNone = Mpeg2Telecine' "NONE"

pattern MTSoft :: Mpeg2Telecine
pattern MTSoft = Mpeg2Telecine' "SOFT"

{-# COMPLETE
  MTHard,
  MTNone,
  MTSoft,
  Mpeg2Telecine' #-}

instance FromText Mpeg2Telecine where
    parser = (Mpeg2Telecine' . mk) <$> takeText

instance ToText Mpeg2Telecine where
    toText (Mpeg2Telecine' ci) = original ci

-- | Represents an enum of /known/ $Mpeg2Telecine.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Mpeg2Telecine where
    toEnum i = case i of
        0 -> MTHard
        1 -> MTNone
        2 -> MTSoft
        _ -> (error . showText) $ "Unknown index for Mpeg2Telecine: " <> toText i
    fromEnum x = case x of
        MTHard -> 0
        MTNone -> 1
        MTSoft -> 2
        Mpeg2Telecine' name -> (error . showText) $ "Unknown Mpeg2Telecine: " <> original name

-- | Represents the bounds of /known/ $Mpeg2Telecine.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Mpeg2Telecine where
    minBound = MTHard
    maxBound = MTSoft

instance Hashable     Mpeg2Telecine
instance NFData       Mpeg2Telecine
instance ToByteString Mpeg2Telecine
instance ToQuery      Mpeg2Telecine
instance ToHeader     Mpeg2Telecine

instance ToJSON Mpeg2Telecine where
    toJSON = toJSONText

instance FromJSON Mpeg2Telecine where
    parseJSON = parseJSONText "Mpeg2Telecine"
