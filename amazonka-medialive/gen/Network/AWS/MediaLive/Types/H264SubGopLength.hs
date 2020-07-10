{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264SubGopLength
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H264SubGopLength (
  H264SubGopLength (
    ..
    , Dynamic
    , Fixed
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | H264 Sub Gop Length
data H264SubGopLength = H264SubGopLength' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern Dynamic :: H264SubGopLength
pattern Dynamic = H264SubGopLength' "DYNAMIC"

pattern Fixed :: H264SubGopLength
pattern Fixed = H264SubGopLength' "FIXED"

{-# COMPLETE
  Dynamic,
  Fixed,
  H264SubGopLength' #-}

instance FromText H264SubGopLength where
    parser = (H264SubGopLength' . mk) <$> takeText

instance ToText H264SubGopLength where
    toText (H264SubGopLength' ci) = original ci

-- | Represents an enum of /known/ $H264SubGopLength.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H264SubGopLength where
    toEnum i = case i of
        0 -> Dynamic
        1 -> Fixed
        _ -> (error . showText) $ "Unknown index for H264SubGopLength: " <> toText i
    fromEnum x = case x of
        Dynamic -> 0
        Fixed -> 1
        H264SubGopLength' name -> (error . showText) $ "Unknown H264SubGopLength: " <> original name

-- | Represents the bounds of /known/ $H264SubGopLength.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H264SubGopLength where
    minBound = Dynamic
    maxBound = Fixed

instance Hashable     H264SubGopLength
instance NFData       H264SubGopLength
instance ToByteString H264SubGopLength
instance ToQuery      H264SubGopLength
instance ToHeader     H264SubGopLength

instance ToJSON H264SubGopLength where
    toJSON = toJSONText

instance FromJSON H264SubGopLength where
    parseJSON = parseJSONText "H264SubGopLength"
