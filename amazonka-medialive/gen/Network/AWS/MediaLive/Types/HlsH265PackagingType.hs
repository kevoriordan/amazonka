{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsH265PackagingType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.HlsH265PackagingType (
  HlsH265PackagingType (
    ..
    , HEV1
    , HVC1
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Hls H265 Packaging Type
data HlsH265PackagingType = HlsH265PackagingType' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern HEV1 :: HlsH265PackagingType
pattern HEV1 = HlsH265PackagingType' "HEV1"

pattern HVC1 :: HlsH265PackagingType
pattern HVC1 = HlsH265PackagingType' "HVC1"

{-# COMPLETE
  HEV1,
  HVC1,
  HlsH265PackagingType' #-}

instance FromText HlsH265PackagingType where
    parser = (HlsH265PackagingType' . mk) <$> takeText

instance ToText HlsH265PackagingType where
    toText (HlsH265PackagingType' ci) = original ci

-- | Represents an enum of /known/ $HlsH265PackagingType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HlsH265PackagingType where
    toEnum i = case i of
        0 -> HEV1
        1 -> HVC1
        _ -> (error . showText) $ "Unknown index for HlsH265PackagingType: " <> toText i
    fromEnum x = case x of
        HEV1 -> 0
        HVC1 -> 1
        HlsH265PackagingType' name -> (error . showText) $ "Unknown HlsH265PackagingType: " <> original name

-- | Represents the bounds of /known/ $HlsH265PackagingType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HlsH265PackagingType where
    minBound = HEV1
    maxBound = HVC1

instance Hashable     HlsH265PackagingType
instance NFData       HlsH265PackagingType
instance ToByteString HlsH265PackagingType
instance ToQuery      HlsH265PackagingType
instance ToHeader     HlsH265PackagingType

instance ToJSON HlsH265PackagingType where
    toJSON = toJSONText

instance FromJSON HlsH265PackagingType where
    parseJSON = parseJSONText "HlsH265PackagingType"
