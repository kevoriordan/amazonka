{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ParquetCompression
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.ParquetCompression (
  ParquetCompression (
    ..
    , PCGzip
    , PCSnappy
    , PCUncompressed
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ParquetCompression = ParquetCompression' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern PCGzip :: ParquetCompression
pattern PCGzip = ParquetCompression' "GZIP"

pattern PCSnappy :: ParquetCompression
pattern PCSnappy = ParquetCompression' "SNAPPY"

pattern PCUncompressed :: ParquetCompression
pattern PCUncompressed = ParquetCompression' "UNCOMPRESSED"

{-# COMPLETE
  PCGzip,
  PCSnappy,
  PCUncompressed,
  ParquetCompression' #-}

instance FromText ParquetCompression where
    parser = (ParquetCompression' . mk) <$> takeText

instance ToText ParquetCompression where
    toText (ParquetCompression' ci) = original ci

-- | Represents an enum of /known/ $ParquetCompression.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ParquetCompression where
    toEnum i = case i of
        0 -> PCGzip
        1 -> PCSnappy
        2 -> PCUncompressed
        _ -> (error . showText) $ "Unknown index for ParquetCompression: " <> toText i
    fromEnum x = case x of
        PCGzip -> 0
        PCSnappy -> 1
        PCUncompressed -> 2
        ParquetCompression' name -> (error . showText) $ "Unknown ParquetCompression: " <> original name

-- | Represents the bounds of /known/ $ParquetCompression.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ParquetCompression where
    minBound = PCGzip
    maxBound = PCUncompressed

instance Hashable     ParquetCompression
instance NFData       ParquetCompression
instance ToByteString ParquetCompression
instance ToQuery      ParquetCompression
instance ToHeader     ParquetCompression

instance ToJSON ParquetCompression where
    toJSON = toJSONText

instance FromJSON ParquetCompression where
    parseJSON = parseJSONText "ParquetCompression"
