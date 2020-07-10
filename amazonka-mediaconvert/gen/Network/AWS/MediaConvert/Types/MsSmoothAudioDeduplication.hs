{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MsSmoothAudioDeduplication
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.MsSmoothAudioDeduplication (
  MsSmoothAudioDeduplication (
    ..
    , MSADCombineDuplicateStreams
    , MSADNone
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | COMBINE_DUPLICATE_STREAMS combines identical audio encoding settings across a Microsoft Smooth output group into a single audio stream.
data MsSmoothAudioDeduplication = MsSmoothAudioDeduplication' (CI
                                                                 Text)
                                    deriving (Eq, Ord, Read, Show, Data,
                                              Typeable, Generic)

pattern MSADCombineDuplicateStreams :: MsSmoothAudioDeduplication
pattern MSADCombineDuplicateStreams = MsSmoothAudioDeduplication' "COMBINE_DUPLICATE_STREAMS"

pattern MSADNone :: MsSmoothAudioDeduplication
pattern MSADNone = MsSmoothAudioDeduplication' "NONE"

{-# COMPLETE
  MSADCombineDuplicateStreams,
  MSADNone,
  MsSmoothAudioDeduplication' #-}

instance FromText MsSmoothAudioDeduplication where
    parser = (MsSmoothAudioDeduplication' . mk) <$> takeText

instance ToText MsSmoothAudioDeduplication where
    toText (MsSmoothAudioDeduplication' ci) = original ci

-- | Represents an enum of /known/ $MsSmoothAudioDeduplication.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MsSmoothAudioDeduplication where
    toEnum i = case i of
        0 -> MSADCombineDuplicateStreams
        1 -> MSADNone
        _ -> (error . showText) $ "Unknown index for MsSmoothAudioDeduplication: " <> toText i
    fromEnum x = case x of
        MSADCombineDuplicateStreams -> 0
        MSADNone -> 1
        MsSmoothAudioDeduplication' name -> (error . showText) $ "Unknown MsSmoothAudioDeduplication: " <> original name

-- | Represents the bounds of /known/ $MsSmoothAudioDeduplication.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MsSmoothAudioDeduplication where
    minBound = MSADCombineDuplicateStreams
    maxBound = MSADNone

instance Hashable     MsSmoothAudioDeduplication
instance NFData       MsSmoothAudioDeduplication
instance ToByteString MsSmoothAudioDeduplication
instance ToQuery      MsSmoothAudioDeduplication
instance ToHeader     MsSmoothAudioDeduplication

instance ToJSON MsSmoothAudioDeduplication where
    toJSON = toJSONText

instance FromJSON MsSmoothAudioDeduplication where
    parseJSON = parseJSONText "MsSmoothAudioDeduplication"
