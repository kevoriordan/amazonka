{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ImscStylePassthrough
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.ImscStylePassthrough (
  ImscStylePassthrough (
    ..
    , ISPDisabled
    , ISPEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Keep this setting enabled to have MediaConvert use the font style and position information from the captions source in the output. This option is available only when your input captions are IMSC, SMPTE-TT, or TTML. Disable this setting for simplified output captions.
data ImscStylePassthrough = ImscStylePassthrough' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern ISPDisabled :: ImscStylePassthrough
pattern ISPDisabled = ImscStylePassthrough' "DISABLED"

pattern ISPEnabled :: ImscStylePassthrough
pattern ISPEnabled = ImscStylePassthrough' "ENABLED"

{-# COMPLETE
  ISPDisabled,
  ISPEnabled,
  ImscStylePassthrough' #-}

instance FromText ImscStylePassthrough where
    parser = (ImscStylePassthrough' . mk) <$> takeText

instance ToText ImscStylePassthrough where
    toText (ImscStylePassthrough' ci) = original ci

-- | Represents an enum of /known/ $ImscStylePassthrough.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ImscStylePassthrough where
    toEnum i = case i of
        0 -> ISPDisabled
        1 -> ISPEnabled
        _ -> (error . showText) $ "Unknown index for ImscStylePassthrough: " <> toText i
    fromEnum x = case x of
        ISPDisabled -> 0
        ISPEnabled -> 1
        ImscStylePassthrough' name -> (error . showText) $ "Unknown ImscStylePassthrough: " <> original name

-- | Represents the bounds of /known/ $ImscStylePassthrough.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ImscStylePassthrough where
    minBound = ISPDisabled
    maxBound = ISPEnabled

instance Hashable     ImscStylePassthrough
instance NFData       ImscStylePassthrough
instance ToByteString ImscStylePassthrough
instance ToQuery      ImscStylePassthrough
instance ToHeader     ImscStylePassthrough

instance ToJSON ImscStylePassthrough where
    toJSON = toJSONText

instance FromJSON ImscStylePassthrough where
    parseJSON = parseJSONText "ImscStylePassthrough"
