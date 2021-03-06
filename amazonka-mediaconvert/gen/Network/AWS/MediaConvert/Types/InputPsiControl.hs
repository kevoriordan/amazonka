{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.InputPsiControl
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.InputPsiControl (
  InputPsiControl (
    ..
    , IgnorePsi
    , UsePsi
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Set PSI control (InputPsiControl) for transport stream inputs to specify which data the demux process to scans. * Ignore PSI - Scan all PIDs for audio and video. * Use PSI - Scan only PSI data.
data InputPsiControl = InputPsiControl' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern IgnorePsi :: InputPsiControl
pattern IgnorePsi = InputPsiControl' "IGNORE_PSI"

pattern UsePsi :: InputPsiControl
pattern UsePsi = InputPsiControl' "USE_PSI"

{-# COMPLETE
  IgnorePsi,
  UsePsi,
  InputPsiControl' #-}

instance FromText InputPsiControl where
    parser = (InputPsiControl' . mk) <$> takeText

instance ToText InputPsiControl where
    toText (InputPsiControl' ci) = original ci

-- | Represents an enum of /known/ $InputPsiControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InputPsiControl where
    toEnum i = case i of
        0 -> IgnorePsi
        1 -> UsePsi
        _ -> (error . showText) $ "Unknown index for InputPsiControl: " <> toText i
    fromEnum x = case x of
        IgnorePsi -> 0
        UsePsi -> 1
        InputPsiControl' name -> (error . showText) $ "Unknown InputPsiControl: " <> original name

-- | Represents the bounds of /known/ $InputPsiControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InputPsiControl where
    minBound = IgnorePsi
    maxBound = UsePsi

instance Hashable     InputPsiControl
instance NFData       InputPsiControl
instance ToByteString InputPsiControl
instance ToQuery      InputPsiControl
instance ToHeader     InputPsiControl

instance ToJSON InputPsiControl where
    toJSON = toJSONText

instance FromJSON InputPsiControl where
    parseJSON = parseJSONText "InputPsiControl"
