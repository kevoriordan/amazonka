{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.DocumentClassifierMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.DocumentClassifierMode (
  DocumentClassifierMode (
    ..
    , MultiClass
    , MultiLabel
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DocumentClassifierMode = DocumentClassifierMode' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern MultiClass :: DocumentClassifierMode
pattern MultiClass = DocumentClassifierMode' "MULTI_CLASS"

pattern MultiLabel :: DocumentClassifierMode
pattern MultiLabel = DocumentClassifierMode' "MULTI_LABEL"

{-# COMPLETE
  MultiClass,
  MultiLabel,
  DocumentClassifierMode' #-}

instance FromText DocumentClassifierMode where
    parser = (DocumentClassifierMode' . mk) <$> takeText

instance ToText DocumentClassifierMode where
    toText (DocumentClassifierMode' ci) = original ci

-- | Represents an enum of /known/ $DocumentClassifierMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DocumentClassifierMode where
    toEnum i = case i of
        0 -> MultiClass
        1 -> MultiLabel
        _ -> (error . showText) $ "Unknown index for DocumentClassifierMode: " <> toText i
    fromEnum x = case x of
        MultiClass -> 0
        MultiLabel -> 1
        DocumentClassifierMode' name -> (error . showText) $ "Unknown DocumentClassifierMode: " <> original name

-- | Represents the bounds of /known/ $DocumentClassifierMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DocumentClassifierMode where
    minBound = MultiClass
    maxBound = MultiLabel

instance Hashable     DocumentClassifierMode
instance NFData       DocumentClassifierMode
instance ToByteString DocumentClassifierMode
instance ToQuery      DocumentClassifierMode
instance ToHeader     DocumentClassifierMode

instance ToJSON DocumentClassifierMode where
    toJSON = toJSONText

instance FromJSON DocumentClassifierMode where
    parseJSON = parseJSONText "DocumentClassifierMode"
