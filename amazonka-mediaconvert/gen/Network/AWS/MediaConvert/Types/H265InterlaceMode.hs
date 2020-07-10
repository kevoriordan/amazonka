{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265InterlaceMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H265InterlaceMode (
  H265InterlaceMode (
    ..
    , BottomField
    , FollowBottomField
    , FollowTopField
    , Progressive
    , TopField
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Choose the scan line type for the output. Choose Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Choose Top Field First (TOP_FIELD) or Bottom Field First (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Choose Follow, Default Top (FOLLOW_TOP_FIELD) or Follow, Default Bottom (FOLLOW_BOTTOM_FIELD) to create an interlaced output with the same field polarity as the source. If the source is interlaced, the output will be interlaced with the same polarity as the source (it will follow the source). The output could therefore be a mix of "top field first" and "bottom field first". If the source is progressive, your output will be interlaced with "top field first" or "bottom field first" polarity, depending on which of the Follow options you chose. If you don't choose a value, the service will default to Progressive (PROGRESSIVE).
data H265InterlaceMode = H265InterlaceMode' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern BottomField :: H265InterlaceMode
pattern BottomField = H265InterlaceMode' "BOTTOM_FIELD"

pattern FollowBottomField :: H265InterlaceMode
pattern FollowBottomField = H265InterlaceMode' "FOLLOW_BOTTOM_FIELD"

pattern FollowTopField :: H265InterlaceMode
pattern FollowTopField = H265InterlaceMode' "FOLLOW_TOP_FIELD"

pattern Progressive :: H265InterlaceMode
pattern Progressive = H265InterlaceMode' "PROGRESSIVE"

pattern TopField :: H265InterlaceMode
pattern TopField = H265InterlaceMode' "TOP_FIELD"

{-# COMPLETE
  BottomField,
  FollowBottomField,
  FollowTopField,
  Progressive,
  TopField,
  H265InterlaceMode' #-}

instance FromText H265InterlaceMode where
    parser = (H265InterlaceMode' . mk) <$> takeText

instance ToText H265InterlaceMode where
    toText (H265InterlaceMode' ci) = original ci

-- | Represents an enum of /known/ $H265InterlaceMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H265InterlaceMode where
    toEnum i = case i of
        0 -> BottomField
        1 -> FollowBottomField
        2 -> FollowTopField
        3 -> Progressive
        4 -> TopField
        _ -> (error . showText) $ "Unknown index for H265InterlaceMode: " <> toText i
    fromEnum x = case x of
        BottomField -> 0
        FollowBottomField -> 1
        FollowTopField -> 2
        Progressive -> 3
        TopField -> 4
        H265InterlaceMode' name -> (error . showText) $ "Unknown H265InterlaceMode: " <> original name

-- | Represents the bounds of /known/ $H265InterlaceMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H265InterlaceMode where
    minBound = BottomField
    maxBound = TopField

instance Hashable     H265InterlaceMode
instance NFData       H265InterlaceMode
instance ToByteString H265InterlaceMode
instance ToQuery      H265InterlaceMode
instance ToHeader     H265InterlaceMode

instance ToJSON H265InterlaceMode where
    toJSON = toJSONText

instance FromJSON H265InterlaceMode where
    parseJSON = parseJSONText "H265InterlaceMode"
