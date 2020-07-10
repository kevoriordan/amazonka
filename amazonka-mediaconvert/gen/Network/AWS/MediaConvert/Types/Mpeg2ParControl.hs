{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2ParControl
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Mpeg2ParControl (
  Mpeg2ParControl (
    ..
    , MPCInitializeFromSource
    , MPCSpecified
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Using the API, enable ParFollowSource if you want the service to use the pixel aspect ratio from the input. Using the console, do this by choosing Follow source for Pixel aspect ratio.
data Mpeg2ParControl = Mpeg2ParControl' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern MPCInitializeFromSource :: Mpeg2ParControl
pattern MPCInitializeFromSource = Mpeg2ParControl' "INITIALIZE_FROM_SOURCE"

pattern MPCSpecified :: Mpeg2ParControl
pattern MPCSpecified = Mpeg2ParControl' "SPECIFIED"

{-# COMPLETE
  MPCInitializeFromSource,
  MPCSpecified,
  Mpeg2ParControl' #-}

instance FromText Mpeg2ParControl where
    parser = (Mpeg2ParControl' . mk) <$> takeText

instance ToText Mpeg2ParControl where
    toText (Mpeg2ParControl' ci) = original ci

-- | Represents an enum of /known/ $Mpeg2ParControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Mpeg2ParControl where
    toEnum i = case i of
        0 -> MPCInitializeFromSource
        1 -> MPCSpecified
        _ -> (error . showText) $ "Unknown index for Mpeg2ParControl: " <> toText i
    fromEnum x = case x of
        MPCInitializeFromSource -> 0
        MPCSpecified -> 1
        Mpeg2ParControl' name -> (error . showText) $ "Unknown Mpeg2ParControl: " <> original name

-- | Represents the bounds of /known/ $Mpeg2ParControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Mpeg2ParControl where
    minBound = MPCInitializeFromSource
    maxBound = MPCSpecified

instance Hashable     Mpeg2ParControl
instance NFData       Mpeg2ParControl
instance ToByteString Mpeg2ParControl
instance ToQuery      Mpeg2ParControl
instance ToHeader     Mpeg2ParControl

instance ToJSON Mpeg2ParControl where
    toJSON = toJSONText

instance FromJSON Mpeg2ParControl where
    parseJSON = parseJSONText "Mpeg2ParControl"
