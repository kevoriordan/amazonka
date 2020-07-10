{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentFormat
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.DocumentFormat (
  DocumentFormat (
    ..
    , JSON
    , Text
    , Yaml
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DocumentFormat = DocumentFormat' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern JSON :: DocumentFormat
pattern JSON = DocumentFormat' "JSON"

pattern Text :: DocumentFormat
pattern Text = DocumentFormat' "TEXT"

pattern Yaml :: DocumentFormat
pattern Yaml = DocumentFormat' "YAML"

{-# COMPLETE
  JSON,
  Text,
  Yaml,
  DocumentFormat' #-}

instance FromText DocumentFormat where
    parser = (DocumentFormat' . mk) <$> takeText

instance ToText DocumentFormat where
    toText (DocumentFormat' ci) = original ci

-- | Represents an enum of /known/ $DocumentFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DocumentFormat where
    toEnum i = case i of
        0 -> JSON
        1 -> Text
        2 -> Yaml
        _ -> (error . showText) $ "Unknown index for DocumentFormat: " <> toText i
    fromEnum x = case x of
        JSON -> 0
        Text -> 1
        Yaml -> 2
        DocumentFormat' name -> (error . showText) $ "Unknown DocumentFormat: " <> original name

-- | Represents the bounds of /known/ $DocumentFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DocumentFormat where
    minBound = JSON
    maxBound = Yaml

instance Hashable     DocumentFormat
instance NFData       DocumentFormat
instance ToByteString DocumentFormat
instance ToQuery      DocumentFormat
instance ToHeader     DocumentFormat

instance ToJSON DocumentFormat where
    toJSON = toJSONText

instance FromJSON DocumentFormat where
    parseJSON = parseJSONText "DocumentFormat"
