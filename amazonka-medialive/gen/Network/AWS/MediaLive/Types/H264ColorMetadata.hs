{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264ColorMetadata
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H264ColorMetadata (
  H264ColorMetadata (
    ..
    , HIgnore
    , HInsert
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | H264 Color Metadata
data H264ColorMetadata = H264ColorMetadata' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern HIgnore :: H264ColorMetadata
pattern HIgnore = H264ColorMetadata' "IGNORE"

pattern HInsert :: H264ColorMetadata
pattern HInsert = H264ColorMetadata' "INSERT"

{-# COMPLETE
  HIgnore,
  HInsert,
  H264ColorMetadata' #-}

instance FromText H264ColorMetadata where
    parser = (H264ColorMetadata' . mk) <$> takeText

instance ToText H264ColorMetadata where
    toText (H264ColorMetadata' ci) = original ci

-- | Represents an enum of /known/ $H264ColorMetadata.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H264ColorMetadata where
    toEnum i = case i of
        0 -> HIgnore
        1 -> HInsert
        _ -> (error . showText) $ "Unknown index for H264ColorMetadata: " <> toText i
    fromEnum x = case x of
        HIgnore -> 0
        HInsert -> 1
        H264ColorMetadata' name -> (error . showText) $ "Unknown H264ColorMetadata: " <> original name

-- | Represents the bounds of /known/ $H264ColorMetadata.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H264ColorMetadata where
    minBound = HIgnore
    maxBound = HInsert

instance Hashable     H264ColorMetadata
instance NFData       H264ColorMetadata
instance ToByteString H264ColorMetadata
instance ToQuery      H264ColorMetadata
instance ToHeader     H264ColorMetadata

instance ToJSON H264ColorMetadata where
    toJSON = toJSONText

instance FromJSON H264ColorMetadata where
    parseJSON = parseJSONText "H264ColorMetadata"
