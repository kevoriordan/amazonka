{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AccelerationMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AccelerationMode (
  AccelerationMode (
    ..
    , AMDisabled
    , AMEnabled
    , AMPreferred
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Specify whether the service runs your job with accelerated transcoding. Choose DISABLED if you don't want accelerated transcoding. Choose ENABLED if you want your job to run with accelerated transcoding and to fail if your input files or your job settings aren't compatible with accelerated transcoding. Choose PREFERRED if you want your job to run with accelerated transcoding if the job is compatible with the feature and to run at standard speed if it's not.
data AccelerationMode = AccelerationMode' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern AMDisabled :: AccelerationMode
pattern AMDisabled = AccelerationMode' "DISABLED"

pattern AMEnabled :: AccelerationMode
pattern AMEnabled = AccelerationMode' "ENABLED"

pattern AMPreferred :: AccelerationMode
pattern AMPreferred = AccelerationMode' "PREFERRED"

{-# COMPLETE
  AMDisabled,
  AMEnabled,
  AMPreferred,
  AccelerationMode' #-}

instance FromText AccelerationMode where
    parser = (AccelerationMode' . mk) <$> takeText

instance ToText AccelerationMode where
    toText (AccelerationMode' ci) = original ci

-- | Represents an enum of /known/ $AccelerationMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AccelerationMode where
    toEnum i = case i of
        0 -> AMDisabled
        1 -> AMEnabled
        2 -> AMPreferred
        _ -> (error . showText) $ "Unknown index for AccelerationMode: " <> toText i
    fromEnum x = case x of
        AMDisabled -> 0
        AMEnabled -> 1
        AMPreferred -> 2
        AccelerationMode' name -> (error . showText) $ "Unknown AccelerationMode: " <> original name

-- | Represents the bounds of /known/ $AccelerationMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AccelerationMode where
    minBound = AMDisabled
    maxBound = AMPreferred

instance Hashable     AccelerationMode
instance NFData       AccelerationMode
instance ToByteString AccelerationMode
instance ToQuery      AccelerationMode
instance ToHeader     AccelerationMode

instance ToJSON AccelerationMode where
    toJSON = toJSONText

instance FromJSON AccelerationMode where
    parseJSON = parseJSONText "AccelerationMode"
