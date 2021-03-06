{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsDirectoryStructure
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.HlsDirectoryStructure (
  HlsDirectoryStructure (
    ..
    , SingleDirectory
    , SubdirectoryPerStream
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Hls Directory Structure
data HlsDirectoryStructure = HlsDirectoryStructure' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern SingleDirectory :: HlsDirectoryStructure
pattern SingleDirectory = HlsDirectoryStructure' "SINGLE_DIRECTORY"

pattern SubdirectoryPerStream :: HlsDirectoryStructure
pattern SubdirectoryPerStream = HlsDirectoryStructure' "SUBDIRECTORY_PER_STREAM"

{-# COMPLETE
  SingleDirectory,
  SubdirectoryPerStream,
  HlsDirectoryStructure' #-}

instance FromText HlsDirectoryStructure where
    parser = (HlsDirectoryStructure' . mk) <$> takeText

instance ToText HlsDirectoryStructure where
    toText (HlsDirectoryStructure' ci) = original ci

-- | Represents an enum of /known/ $HlsDirectoryStructure.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HlsDirectoryStructure where
    toEnum i = case i of
        0 -> SingleDirectory
        1 -> SubdirectoryPerStream
        _ -> (error . showText) $ "Unknown index for HlsDirectoryStructure: " <> toText i
    fromEnum x = case x of
        SingleDirectory -> 0
        SubdirectoryPerStream -> 1
        HlsDirectoryStructure' name -> (error . showText) $ "Unknown HlsDirectoryStructure: " <> original name

-- | Represents the bounds of /known/ $HlsDirectoryStructure.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HlsDirectoryStructure where
    minBound = SingleDirectory
    maxBound = SubdirectoryPerStream

instance Hashable     HlsDirectoryStructure
instance NFData       HlsDirectoryStructure
instance ToByteString HlsDirectoryStructure
instance ToQuery      HlsDirectoryStructure
instance ToHeader     HlsDirectoryStructure

instance ToJSON HlsDirectoryStructure where
    toJSON = toJSONText

instance FromJSON HlsDirectoryStructure where
    parseJSON = parseJSONText "HlsDirectoryStructure"
