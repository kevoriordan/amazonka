{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsTsFileMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.HlsTsFileMode (
  HlsTsFileMode (
    ..
    , SegmentedFiles
    , SingleFile
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Hls Ts File Mode
data HlsTsFileMode = HlsTsFileMode' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern SegmentedFiles :: HlsTsFileMode
pattern SegmentedFiles = HlsTsFileMode' "SEGMENTED_FILES"

pattern SingleFile :: HlsTsFileMode
pattern SingleFile = HlsTsFileMode' "SINGLE_FILE"

{-# COMPLETE
  SegmentedFiles,
  SingleFile,
  HlsTsFileMode' #-}

instance FromText HlsTsFileMode where
    parser = (HlsTsFileMode' . mk) <$> takeText

instance ToText HlsTsFileMode where
    toText (HlsTsFileMode' ci) = original ci

-- | Represents an enum of /known/ $HlsTsFileMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HlsTsFileMode where
    toEnum i = case i of
        0 -> SegmentedFiles
        1 -> SingleFile
        _ -> (error . showText) $ "Unknown index for HlsTsFileMode: " <> toText i
    fromEnum x = case x of
        SegmentedFiles -> 0
        SingleFile -> 1
        HlsTsFileMode' name -> (error . showText) $ "Unknown HlsTsFileMode: " <> original name

-- | Represents the bounds of /known/ $HlsTsFileMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HlsTsFileMode where
    minBound = SegmentedFiles
    maxBound = SingleFile

instance Hashable     HlsTsFileMode
instance NFData       HlsTsFileMode
instance ToByteString HlsTsFileMode
instance ToQuery      HlsTsFileMode
instance ToHeader     HlsTsFileMode

instance ToJSON HlsTsFileMode where
    toJSON = toJSONText

instance FromJSON HlsTsFileMode where
    parseJSON = parseJSONText "HlsTsFileMode"
