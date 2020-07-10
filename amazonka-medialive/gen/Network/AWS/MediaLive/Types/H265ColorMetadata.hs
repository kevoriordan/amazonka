{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265ColorMetadata
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H265ColorMetadata (
  H265ColorMetadata (
    ..
    , HCMIgnore
    , HCMInsert
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | H265 Color Metadata
data H265ColorMetadata = H265ColorMetadata' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern HCMIgnore :: H265ColorMetadata
pattern HCMIgnore = H265ColorMetadata' "IGNORE"

pattern HCMInsert :: H265ColorMetadata
pattern HCMInsert = H265ColorMetadata' "INSERT"

{-# COMPLETE
  HCMIgnore,
  HCMInsert,
  H265ColorMetadata' #-}

instance FromText H265ColorMetadata where
    parser = (H265ColorMetadata' . mk) <$> takeText

instance ToText H265ColorMetadata where
    toText (H265ColorMetadata' ci) = original ci

-- | Represents an enum of /known/ $H265ColorMetadata.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H265ColorMetadata where
    toEnum i = case i of
        0 -> HCMIgnore
        1 -> HCMInsert
        _ -> (error . showText) $ "Unknown index for H265ColorMetadata: " <> toText i
    fromEnum x = case x of
        HCMIgnore -> 0
        HCMInsert -> 1
        H265ColorMetadata' name -> (error . showText) $ "Unknown H265ColorMetadata: " <> original name

-- | Represents the bounds of /known/ $H265ColorMetadata.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H265ColorMetadata where
    minBound = HCMIgnore
    maxBound = HCMInsert

instance Hashable     H265ColorMetadata
instance NFData       H265ColorMetadata
instance ToByteString H265ColorMetadata
instance ToQuery      H265ColorMetadata
instance ToHeader     H265ColorMetadata

instance ToJSON H265ColorMetadata where
    toJSON = toJSONText

instance FromJSON H265ColorMetadata where
    parseJSON = parseJSONText "H265ColorMetadata"
