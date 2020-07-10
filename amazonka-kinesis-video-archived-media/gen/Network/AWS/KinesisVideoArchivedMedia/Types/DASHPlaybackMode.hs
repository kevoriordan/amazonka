{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.DASHPlaybackMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisVideoArchivedMedia.Types.DASHPlaybackMode (
  DASHPlaybackMode (
    ..
    , DASHPMLive
    , DASHPMLiveReplay
    , DASHPMOnDemand
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DASHPlaybackMode = DASHPlaybackMode' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern DASHPMLive :: DASHPlaybackMode
pattern DASHPMLive = DASHPlaybackMode' "LIVE"

pattern DASHPMLiveReplay :: DASHPlaybackMode
pattern DASHPMLiveReplay = DASHPlaybackMode' "LIVE_REPLAY"

pattern DASHPMOnDemand :: DASHPlaybackMode
pattern DASHPMOnDemand = DASHPlaybackMode' "ON_DEMAND"

{-# COMPLETE
  DASHPMLive,
  DASHPMLiveReplay,
  DASHPMOnDemand,
  DASHPlaybackMode' #-}

instance FromText DASHPlaybackMode where
    parser = (DASHPlaybackMode' . mk) <$> takeText

instance ToText DASHPlaybackMode where
    toText (DASHPlaybackMode' ci) = original ci

-- | Represents an enum of /known/ $DASHPlaybackMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DASHPlaybackMode where
    toEnum i = case i of
        0 -> DASHPMLive
        1 -> DASHPMLiveReplay
        2 -> DASHPMOnDemand
        _ -> (error . showText) $ "Unknown index for DASHPlaybackMode: " <> toText i
    fromEnum x = case x of
        DASHPMLive -> 0
        DASHPMLiveReplay -> 1
        DASHPMOnDemand -> 2
        DASHPlaybackMode' name -> (error . showText) $ "Unknown DASHPlaybackMode: " <> original name

-- | Represents the bounds of /known/ $DASHPlaybackMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DASHPlaybackMode where
    minBound = DASHPMLive
    maxBound = DASHPMOnDemand

instance Hashable     DASHPlaybackMode
instance NFData       DASHPlaybackMode
instance ToByteString DASHPlaybackMode
instance ToQuery      DASHPlaybackMode
instance ToHeader     DASHPlaybackMode

instance ToJSON DASHPlaybackMode where
    toJSON = toJSONText
