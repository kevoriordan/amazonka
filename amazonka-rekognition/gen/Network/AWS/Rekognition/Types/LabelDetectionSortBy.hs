{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.LabelDetectionSortBy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.LabelDetectionSortBy (
  LabelDetectionSortBy (
    ..
    , LDSBName
    , LDSBTimestamp
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LabelDetectionSortBy = LabelDetectionSortBy' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern LDSBName :: LabelDetectionSortBy
pattern LDSBName = LabelDetectionSortBy' "NAME"

pattern LDSBTimestamp :: LabelDetectionSortBy
pattern LDSBTimestamp = LabelDetectionSortBy' "TIMESTAMP"

{-# COMPLETE
  LDSBName,
  LDSBTimestamp,
  LabelDetectionSortBy' #-}

instance FromText LabelDetectionSortBy where
    parser = (LabelDetectionSortBy' . mk) <$> takeText

instance ToText LabelDetectionSortBy where
    toText (LabelDetectionSortBy' ci) = original ci

-- | Represents an enum of /known/ $LabelDetectionSortBy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LabelDetectionSortBy where
    toEnum i = case i of
        0 -> LDSBName
        1 -> LDSBTimestamp
        _ -> (error . showText) $ "Unknown index for LabelDetectionSortBy: " <> toText i
    fromEnum x = case x of
        LDSBName -> 0
        LDSBTimestamp -> 1
        LabelDetectionSortBy' name -> (error . showText) $ "Unknown LabelDetectionSortBy: " <> original name

-- | Represents the bounds of /known/ $LabelDetectionSortBy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LabelDetectionSortBy where
    minBound = LDSBName
    maxBound = LDSBTimestamp

instance Hashable     LabelDetectionSortBy
instance NFData       LabelDetectionSortBy
instance ToByteString LabelDetectionSortBy
instance ToQuery      LabelDetectionSortBy
instance ToHeader     LabelDetectionSortBy

instance ToJSON LabelDetectionSortBy where
    toJSON = toJSONText
