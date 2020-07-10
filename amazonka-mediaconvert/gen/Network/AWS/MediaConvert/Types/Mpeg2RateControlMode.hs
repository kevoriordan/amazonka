{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2RateControlMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Mpeg2RateControlMode (
  Mpeg2RateControlMode (
    ..
    , Cbr
    , Vbr
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Use Rate control mode (Mpeg2RateControlMode) to specifiy whether the bitrate is variable (vbr) or constant (cbr).
data Mpeg2RateControlMode = Mpeg2RateControlMode' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern Cbr :: Mpeg2RateControlMode
pattern Cbr = Mpeg2RateControlMode' "CBR"

pattern Vbr :: Mpeg2RateControlMode
pattern Vbr = Mpeg2RateControlMode' "VBR"

{-# COMPLETE
  Cbr,
  Vbr,
  Mpeg2RateControlMode' #-}

instance FromText Mpeg2RateControlMode where
    parser = (Mpeg2RateControlMode' . mk) <$> takeText

instance ToText Mpeg2RateControlMode where
    toText (Mpeg2RateControlMode' ci) = original ci

-- | Represents an enum of /known/ $Mpeg2RateControlMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Mpeg2RateControlMode where
    toEnum i = case i of
        0 -> Cbr
        1 -> Vbr
        _ -> (error . showText) $ "Unknown index for Mpeg2RateControlMode: " <> toText i
    fromEnum x = case x of
        Cbr -> 0
        Vbr -> 1
        Mpeg2RateControlMode' name -> (error . showText) $ "Unknown Mpeg2RateControlMode: " <> original name

-- | Represents the bounds of /known/ $Mpeg2RateControlMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Mpeg2RateControlMode where
    minBound = Cbr
    maxBound = Vbr

instance Hashable     Mpeg2RateControlMode
instance NFData       Mpeg2RateControlMode
instance ToByteString Mpeg2RateControlMode
instance ToQuery      Mpeg2RateControlMode
instance ToHeader     Mpeg2RateControlMode

instance ToJSON Mpeg2RateControlMode where
    toJSON = toJSONText

instance FromJSON Mpeg2RateControlMode where
    parseJSON = parseJSONText "Mpeg2RateControlMode"
