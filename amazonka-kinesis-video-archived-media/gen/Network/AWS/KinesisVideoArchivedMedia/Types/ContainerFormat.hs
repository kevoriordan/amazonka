{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.ContainerFormat
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisVideoArchivedMedia.Types.ContainerFormat (
  ContainerFormat (
    ..
    , FragmentedMP4
    , MpegTs
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ContainerFormat = ContainerFormat' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern FragmentedMP4 :: ContainerFormat
pattern FragmentedMP4 = ContainerFormat' "FRAGMENTED_MP4"

pattern MpegTs :: ContainerFormat
pattern MpegTs = ContainerFormat' "MPEG_TS"

{-# COMPLETE
  FragmentedMP4,
  MpegTs,
  ContainerFormat' #-}

instance FromText ContainerFormat where
    parser = (ContainerFormat' . mk) <$> takeText

instance ToText ContainerFormat where
    toText (ContainerFormat' ci) = original ci

-- | Represents an enum of /known/ $ContainerFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ContainerFormat where
    toEnum i = case i of
        0 -> FragmentedMP4
        1 -> MpegTs
        _ -> (error . showText) $ "Unknown index for ContainerFormat: " <> toText i
    fromEnum x = case x of
        FragmentedMP4 -> 0
        MpegTs -> 1
        ContainerFormat' name -> (error . showText) $ "Unknown ContainerFormat: " <> original name

-- | Represents the bounds of /known/ $ContainerFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ContainerFormat where
    minBound = FragmentedMP4
    maxBound = MpegTs

instance Hashable     ContainerFormat
instance NFData       ContainerFormat
instance ToByteString ContainerFormat
instance ToQuery      ContainerFormat
instance ToHeader     ContainerFormat

instance ToJSON ContainerFormat where
    toJSON = toJSONText
