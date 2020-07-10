{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M2tsSegmentationStyle
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.M2tsSegmentationStyle (
  M2tsSegmentationStyle (
    ..
    , MaintainCadence
    , ResetCadence
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The segmentation style parameter controls how segmentation markers are inserted into the transport stream. With avails, it is possible that segments may be truncated, which can influence where future segmentation markers are inserted. When a segmentation style of "reset_cadence" is selected and a segment is truncated due to an avail, we will reset the segmentation cadence. This means the subsequent segment will have a duration of of $segmentation_time seconds. When a segmentation style of "maintain_cadence" is selected and a segment is truncated due to an avail, we will not reset the segmentation cadence. This means the subsequent segment will likely be truncated as well. However, all segments after that will have a duration of $segmentation_time seconds. Note that EBP lookahead is a slight exception to this rule.
data M2tsSegmentationStyle = M2tsSegmentationStyle' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern MaintainCadence :: M2tsSegmentationStyle
pattern MaintainCadence = M2tsSegmentationStyle' "MAINTAIN_CADENCE"

pattern ResetCadence :: M2tsSegmentationStyle
pattern ResetCadence = M2tsSegmentationStyle' "RESET_CADENCE"

{-# COMPLETE
  MaintainCadence,
  ResetCadence,
  M2tsSegmentationStyle' #-}

instance FromText M2tsSegmentationStyle where
    parser = (M2tsSegmentationStyle' . mk) <$> takeText

instance ToText M2tsSegmentationStyle where
    toText (M2tsSegmentationStyle' ci) = original ci

-- | Represents an enum of /known/ $M2tsSegmentationStyle.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum M2tsSegmentationStyle where
    toEnum i = case i of
        0 -> MaintainCadence
        1 -> ResetCadence
        _ -> (error . showText) $ "Unknown index for M2tsSegmentationStyle: " <> toText i
    fromEnum x = case x of
        MaintainCadence -> 0
        ResetCadence -> 1
        M2tsSegmentationStyle' name -> (error . showText) $ "Unknown M2tsSegmentationStyle: " <> original name

-- | Represents the bounds of /known/ $M2tsSegmentationStyle.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded M2tsSegmentationStyle where
    minBound = MaintainCadence
    maxBound = ResetCadence

instance Hashable     M2tsSegmentationStyle
instance NFData       M2tsSegmentationStyle
instance ToByteString M2tsSegmentationStyle
instance ToQuery      M2tsSegmentationStyle
instance ToHeader     M2tsSegmentationStyle

instance ToJSON M2tsSegmentationStyle where
    toJSON = toJSONText

instance FromJSON M2tsSegmentationStyle where
    parseJSON = parseJSONText "M2tsSegmentationStyle"
