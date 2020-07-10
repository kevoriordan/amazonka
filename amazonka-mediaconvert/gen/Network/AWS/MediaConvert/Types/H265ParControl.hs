{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265ParControl
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H265ParControl (
  H265ParControl (
    ..
    , HPCInitializeFromSource
    , HPCSpecified
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Using the API, enable ParFollowSource if you want the service to use the pixel aspect ratio from the input. Using the console, do this by choosing Follow source for Pixel aspect ratio.
data H265ParControl = H265ParControl' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern HPCInitializeFromSource :: H265ParControl
pattern HPCInitializeFromSource = H265ParControl' "INITIALIZE_FROM_SOURCE"

pattern HPCSpecified :: H265ParControl
pattern HPCSpecified = H265ParControl' "SPECIFIED"

{-# COMPLETE
  HPCInitializeFromSource,
  HPCSpecified,
  H265ParControl' #-}

instance FromText H265ParControl where
    parser = (H265ParControl' . mk) <$> takeText

instance ToText H265ParControl where
    toText (H265ParControl' ci) = original ci

-- | Represents an enum of /known/ $H265ParControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H265ParControl where
    toEnum i = case i of
        0 -> HPCInitializeFromSource
        1 -> HPCSpecified
        _ -> (error . showText) $ "Unknown index for H265ParControl: " <> toText i
    fromEnum x = case x of
        HPCInitializeFromSource -> 0
        HPCSpecified -> 1
        H265ParControl' name -> (error . showText) $ "Unknown H265ParControl: " <> original name

-- | Represents the bounds of /known/ $H265ParControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H265ParControl where
    minBound = HPCInitializeFromSource
    maxBound = HPCSpecified

instance Hashable     H265ParControl
instance NFData       H265ParControl
instance ToByteString H265ParControl
instance ToQuery      H265ParControl
instance ToHeader     H265ParControl

instance ToJSON H265ParControl where
    toJSON = toJSONText

instance FromJSON H265ParControl where
    parseJSON = parseJSONText "H265ParControl"
