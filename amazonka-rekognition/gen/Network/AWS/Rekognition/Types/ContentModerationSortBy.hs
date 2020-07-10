{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ContentModerationSortBy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.ContentModerationSortBy (
  ContentModerationSortBy (
    ..
    , CMSBName
    , CMSBTimestamp
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ContentModerationSortBy = ContentModerationSortBy' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern CMSBName :: ContentModerationSortBy
pattern CMSBName = ContentModerationSortBy' "NAME"

pattern CMSBTimestamp :: ContentModerationSortBy
pattern CMSBTimestamp = ContentModerationSortBy' "TIMESTAMP"

{-# COMPLETE
  CMSBName,
  CMSBTimestamp,
  ContentModerationSortBy' #-}

instance FromText ContentModerationSortBy where
    parser = (ContentModerationSortBy' . mk) <$> takeText

instance ToText ContentModerationSortBy where
    toText (ContentModerationSortBy' ci) = original ci

-- | Represents an enum of /known/ $ContentModerationSortBy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ContentModerationSortBy where
    toEnum i = case i of
        0 -> CMSBName
        1 -> CMSBTimestamp
        _ -> (error . showText) $ "Unknown index for ContentModerationSortBy: " <> toText i
    fromEnum x = case x of
        CMSBName -> 0
        CMSBTimestamp -> 1
        ContentModerationSortBy' name -> (error . showText) $ "Unknown ContentModerationSortBy: " <> original name

-- | Represents the bounds of /known/ $ContentModerationSortBy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ContentModerationSortBy where
    minBound = CMSBName
    maxBound = CMSBTimestamp

instance Hashable     ContentModerationSortBy
instance NFData       ContentModerationSortBy
instance ToByteString ContentModerationSortBy
instance ToQuery      ContentModerationSortBy
instance ToHeader     ContentModerationSortBy

instance ToJSON ContentModerationSortBy where
    toJSON = toJSONText
