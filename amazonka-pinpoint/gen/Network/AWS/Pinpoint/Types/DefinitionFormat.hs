{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.DefinitionFormat
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.DefinitionFormat (
  DefinitionFormat (
    ..
    , CSV
    , JSON
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DefinitionFormat = DefinitionFormat' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern CSV :: DefinitionFormat
pattern CSV = DefinitionFormat' "CSV"

pattern JSON :: DefinitionFormat
pattern JSON = DefinitionFormat' "JSON"

{-# COMPLETE
  CSV,
  JSON,
  DefinitionFormat' #-}

instance FromText DefinitionFormat where
    parser = (DefinitionFormat' . mk) <$> takeText

instance ToText DefinitionFormat where
    toText (DefinitionFormat' ci) = original ci

-- | Represents an enum of /known/ $DefinitionFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DefinitionFormat where
    toEnum i = case i of
        0 -> CSV
        1 -> JSON
        _ -> (error . showText) $ "Unknown index for DefinitionFormat: " <> toText i
    fromEnum x = case x of
        CSV -> 0
        JSON -> 1
        DefinitionFormat' name -> (error . showText) $ "Unknown DefinitionFormat: " <> original name

-- | Represents the bounds of /known/ $DefinitionFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DefinitionFormat where
    minBound = CSV
    maxBound = JSON

instance Hashable     DefinitionFormat
instance NFData       DefinitionFormat
instance ToByteString DefinitionFormat
instance ToQuery      DefinitionFormat
instance ToHeader     DefinitionFormat

instance ToJSON DefinitionFormat where
    toJSON = toJSONText

instance FromJSON DefinitionFormat where
    parseJSON = parseJSONText "DefinitionFormat"
