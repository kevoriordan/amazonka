{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ExportDataFormat
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Discovery.Types.ExportDataFormat (
  ExportDataFormat (
    ..
    , CSV
    , Graphml
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ExportDataFormat = ExportDataFormat' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern CSV :: ExportDataFormat
pattern CSV = ExportDataFormat' "CSV"

pattern Graphml :: ExportDataFormat
pattern Graphml = ExportDataFormat' "GRAPHML"

{-# COMPLETE
  CSV,
  Graphml,
  ExportDataFormat' #-}

instance FromText ExportDataFormat where
    parser = (ExportDataFormat' . mk) <$> takeText

instance ToText ExportDataFormat where
    toText (ExportDataFormat' ci) = original ci

-- | Represents an enum of /known/ $ExportDataFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ExportDataFormat where
    toEnum i = case i of
        0 -> CSV
        1 -> Graphml
        _ -> (error . showText) $ "Unknown index for ExportDataFormat: " <> toText i
    fromEnum x = case x of
        CSV -> 0
        Graphml -> 1
        ExportDataFormat' name -> (error . showText) $ "Unknown ExportDataFormat: " <> original name

-- | Represents the bounds of /known/ $ExportDataFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ExportDataFormat where
    minBound = CSV
    maxBound = Graphml

instance Hashable     ExportDataFormat
instance NFData       ExportDataFormat
instance ToByteString ExportDataFormat
instance ToQuery      ExportDataFormat
instance ToHeader     ExportDataFormat

instance ToJSON ExportDataFormat where
    toJSON = toJSONText
